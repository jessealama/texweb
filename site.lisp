(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (d '(cl-who hunchentoot cl-fad bordeaux-threads))
    (asdf:oos 'asdf:load-op d)))

(defpackage :texserv
  (:use :cl
	:cl-who
	:cl-fad
	:bt
	:hunchentoot))

(in-package :texserv)

(setq *dispatch-table*
      `(,(create-prefix-dispatcher "/start" 'start-page)
	,(create-prefix-dispatcher "/about" 'about-page)
	,(create-prefix-dispatcher "/upload" 'upload-page)
	,(create-prefix-dispatcher "/compile" 'compile-page)))

;;; Running programs

;;; Logging

(setf *message-log-pathname* "/tmp/texserv/messages")
(setf *access-log-pathname* "/tmp/texserv/access")
(setf *log-lisp-errors-p* t)
(setf *log-lisp-backtraces-p* t)

(defvar sandbox-root "/tmp/texserv"
  "The directory under which uploaded user data is stored.")

(setq *prologue*
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")

(defmacro with-xml-declaration (&body body)
  `(concatenate 'string
     "<?xml version='1.1' encoding='UTF-8'?>"
     (format nil "~%")
     *prologue*
     (with-html-output-to-string (*standard-output* nil :indent t)
       ,@body)))

(defmacro with-html (&body body)
  `(with-xml-declaration
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	,@body)))

(defmacro with-title ((title) &body body)
  `(with-html
     (:head (:title ,title))
     (:body ,@body)))

(defmacro define-xml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "application/xml+xhtml")
     ,@body))

(defmacro define-xhtml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "text/html")
     ,@body))

;; /about
(define-xhtml-handler about-page ()
  (with-title ("About this service")
    (:h1 "What this service provides")
    (:p
"With the serivce provided by this site, you can upload your TeX
work (TeX, LaTeX, ConTeXt, BibTeX, fonts, hyphenation patterns) to our
server, compile the finished product, and download the results.")
    (:p 
"By default, Portable Document Format (PDF) output is generated.
Optionally, one can specify PostScript (PS) and Device
Independent (DVI) output as well.")
    (:h1 "What one needs to use this service")
    (:p 
"You need a modern web browser.  This web site adheres the the XHTML
1.1 document type. Although this document type specification is nearly
ten years old, support for it might be irregular among older browsers.
It is known to work with newer versions of Opera, Safari, Firefox,
Camino, and Chrome.  There are all sorts of browsers with which this
site works. There is one exception: Internet Explorer version 6 (and
below). If you are using Internet Explorer, please use version 7 or 8.
Version 6 of Internet Explorer (and below) is not supported.")
    (:h1 "How this service works")
    (:h1 "How your uploaded files are stored")
    (:p
"Your uploaded work (TeX files, BibTeX files, etc.), and the results
of compiling your work, will be stored on the server for at most one hour.
After one hour, your files may be deleted at any time.")
    (:h1 "About security")
    (:p
"There are two aspects of security that you should be aware of: your
own security, and ours.  If you are impatient, here is a summary:")
    (:p
(:b "USE THIS TOOL AT YOUR OWN RISK."))
    (:p 
"Concerning your security:")
    (:ul
     (:li 
"When uploading your work to this service, your work is not
encrypted.")
     (:li
"Just as your data is not encrypted during transmission from your
machine to our server, your data is stored on our server without
encryption.  We intend that different users of this web site are able
to access only their own data, but we cannot promise that this is
always the case; bugs or errors in TeX and friends, or in the programs
that underlie this web site, could conceivably lead to your data being
unintentionaly shared with other users whose intentions may not be
noble.")
     (:li
"Your work resides on our server in an unencrypted form.  Your files
may thus become accessed and stored by various services running on our
server, such as automated backup scanners and search databases.  We
intend to block your data from such tools, but we cannot promise that
some tool, without our knowledge, would access your data.")
    (:li
"If your browser supports it, this site uses cookies.  If your browser
doesn't support cookies, or if you have disabled support for cookies
in your browser, this site will work.  But if cookies are enabled,
our server will know that, and they will be used."))
    (:p "Concerning our own security:")
    (:ul
     (:li
"To prevent abuse of our network resources, we limit the sizes of the
files that can be uploaded onto our server.  The limit is " (:b "10 MB.")
"Requests to transmit TeX files or any other file above this size will
be denied.")
     (:li
"To prevent abuse of our storage facilities, we limit the size of the
files that can be generated.  The limit is " (:b "25 MB.")
"If a file is generated whose size exceeds 25 MB, compilation will be
aborted.")
    (:li
"You can upload at most" (:b "25") "files.")
    (:li
"We store data for at most" (:b "10") "users. If there are ten active
sessions and a new user comes to this site, the data for the oldest
session will be deleted."))
   (:p
"If you are trying to carry out reasonable TeX work but these
limitations prevent you from completing your task, please write to us
and explain your situation.")))

(defvar sessions (make-hash-table)
  "A table of all the active sessions we are tracking.  Keys are names
of the session, i.e., the values of NAME variables in the session cookies.")

(defvar max-number-of-submitted-files 25
  "The number of files that a user is permitted to upload.")

(defvar max-file-size 10000000
  "The size of the largest file we will accept.")

(defun file-size (path)
  (let ((s (ignore-errors
	     (with-open-file (in path)
	       (file-length in)))))
    (or s -1)))

(defvar session-uploads (make-hash-table)
  "A mapping from session names to lists of paths, saying which files
have been already uploaded for the session.")

(defun handle-file (post-parameter)
  (if post-parameter
      (if (listp post-parameter)
	  (destructuring-bind (path file-name content-type)
	      post-parameter
	    (declare (ignore content-type)) ;; don't know how to use this info
	    (if (session-verify *request*)
		(let ((session-id (gethash *session* hunchentoot-sessions->ids)))
		  (warn "the value of the session id is ~A" session-id)
		  (if session-id
		      (let ((num-already-submitted (gethash session-id
							    sessions)))
			(if (or (null num-already-submitted)
				(< num-already-submitted
				   max-number-of-submitted-files))
			    (let ((previously-submitted 
				   (gethash session-id session-uploads)))
			      (if (string= file-name "")
				  :empty-file-name
				  (if (member file-name 
					      previously-submitted 
					      :test #'string=)
				      :duplicate-filename
				      (let ((size (file-size path)))
					(if (and (> size 0)
						 (< size max-file-size))
					    (let ((session-root 
						   (concatenate 'string
								sandbox-root
								"/"
								(format nil "~A" session-id)
								"/")))
					      (unless (directory-exists-p session-root)
						(ensure-directories-exist session-root))
					      (warn "session root is now ~A" session-root)
					      (let ((new-path 
						     (concatenate 'string 
								  session-root
								  file-name)))
						; we need to sanitize this and/or
					        ; block bad inputs
						(rename-file path new-path)
						(setf (gethash session-id 
							       session-uploads)
						      (cons file-name 
							    (gethash session-id
								     session-uploads)))
						(if (null (gethash session-id 
								   sessions))
						    (setf (gethash session-id 
								   sessions) 1)
						    (incf (gethash session-id
								   sessions)))
						:ok))
					    :file-too-large)))))
		      :too-many-submitted-files))
		      :null-session-id))
		:verify-session-failure))
	  post-parameter)
      :null-post-parameter))

(defun random-session-name ()
  (let ((s (make-string 40)))
    (dotimes (i 40 s)
      (setf (aref s i)
	    (case (random 16)
	      (0 #\0)
	      (1 #\1)
	      (2 #\2)
	      (3 #\3)
	      (4 #\4)
	      (5 #\5)
	      (6 #\6)
	      (7 #\7)
	      (8 #\8)
	      (9 #\9)
	      (10 #\a)
	      (11 #\b)
	      (12 #\c)
	      (13 #\d)
	      (14 #\e)
	      (otherwise #\f))))))

(defmethod session-cookie-name ((acceptor acceptor))
  (declare (ignore acceptor))
  "texserv")

(defvar current-session-id -1)
(defvar session-id-lock (make-lock "texserv"))
(defvar hunchentoot-sessions->ids (make-hash-table))

(setq *rewrite-for-session-urls* nil)
(setq *session-max-time* 3600) ; one hour
(setq *use-remote-addr-for-sessions* t)
(setq *use-user-agent-for-sessions* t)
; (setf (header-out :server) "web server 0.2") ; don't reveal the name of our web server

;; Garbage collection
(defun gc-session (session)
  (let ((our-id (gethash session hunchentoot-sessions->ids)))
    (if our-id
	(let* ((sandbox-dir (pathname-as-directory
			     (concatenate 'string
					  sandbox-root
					  "/"
					  (format nil "~A" our-id)))))
	  (cond ((directory-exists-p sandbox-dir)
		 (delete-directory-and-files sandbox-dir)
		 (ensure-directories-exist sandbox-dir))
		(t
		 (error 
"Error cleaning up session ~A, which has ID ~A and maps to directory ~A:~%the directory does not exist!" session our-id sandbox-dir))))
	(error
"The session ~A does not have an ID!" session))))

(setf *session-removal-hook* #'gc-session)
(setq *session-gc-frequency* 10)

(defun maybe-parse-integer (str)
  (if str
      (parse-integer str)
      0))

(defmacro uploads-table-checkbox-form (label-text)
  (let ((session-id (gensym))
	(uploads (gensym)))
    `(let* ((,session-id (gethash *session* hunchentoot-sessions->ids))
	    (,uploads (gethash ,session-id session-uploads)))
     (htm 
      (:table
       (:tr
	(:th "Filename")
	(:th ,label-text))
       (dolist (file ,uploads)
	 (htm 
	  (:tr
	   (:td (fmt "~A" file))
	   (:td
	    (:label :for (fmt "~A" file)
		    ,label-text)
	    (:input :type "checkbox"
		    :id (fmt "~A" file)
		    :name (fmt "~A" file)))))))))))

(defvar upload-empty-file-name-message
  "The empty string cannot be the name of a file; please try again.")
(defvar duplicate-file-name-message
  "You are trying to upload a file whose name is identical to a file
that you have already uploaded.  It's unclear how to proceed.  Did you
do this by mistake?  If so, then no action is needed; continue to
select additional files to upload, or proceed to compilation.  Are you
trying to upload an updated version of the file you previously
uploaded?  If so, first delete the old file with this name, then
try uploading again.")
(defvar file-too-large-message
  (concatenate 'string
	       "The file you uploaded is too large (its size is greater than"
	       (fmt "~A" max-file-size) "bytes)."))

(defvar too-many-submitted-files-message
  (concatenate 'string
	       "You have already submitted" 
	       (fmt "~A" max-number-of-submitted-files)
	       "files; submitting  more is not permitted."))

(defvar null-session-id-message 
  (let (s)
    (with-html-output-to-string (s)
      "You are visiting this site without first obtaining a proper cookie.
Please visit" (:a :href "start" "the start page") "to get one; from
there you can follow a link to come back here.")))

(defvar verify-session-failure-message
  "Your session with this site is in a strange state: either you are
connecting now with a different web browser than the one you started
this session with, or your IP address now differs from the one you
started with.  Something is fishy; unable to proceed.")

;;; Handlers
;; /start
(define-xhtml-handler upload-page ()
  ;; check to see if the incoming request is too big
  (let* ((length-str (header-in* :content-length))
	 (length (maybe-parse-integer length-str)))
    (if (> length max-file-size)
	(with-title ("Too huge")
	  (:h1 "Joker"))
	(let ((just-getting-started nil)
	      (session-id (gethash *session* hunchentoot-sessions->ids))
	      (handle-result (handle-file (post-parameter "file"))))
	(let ((uploads (gethash session-id session-uploads)))
	  (when (or (null uploads)
		    (zerop (hash-table-count session-uploads))
		    (null handle-result)) ;; not sure about this disjunction
	    (setf just-getting-started t))
	  (with-title ((format nil "Upload TeX data: ~A" handle-result))
	    (:div :class "messages"
		  (:p
		   (case handle-result
		     (:null-post-parameter 
		      (if just-getting-started
			  (htm "Let's get started!")
			  (htm "Nothing submitted; please try again.")))
		     (:ok 
		      (htm "Upload more data?"))
		     (:empty-file-name 
		      (htm upload-empty-file-name-message))
		     (:duplicate-filename 
		      (htm duplicate-file-name-message))
		     (:file-too-large 
		      (htm file-too-large-message))
                     (:too-many-submitted-files 
		      (htm too-many-submitted-files-message))
                     (:null-session-id 
		      (htm null-session-id-message))
                     (:verify-session-failure 
		      (htm verify-session-failure-message))
                     (otherwise 
		      (htm  "Uh oh, something is weird.  Received" 
			    (fmt "~A" handle-result) 
			    "from HANDLE-FILE.")))))
                   (when (and session-id uploads)
                     (htm
                       (:div :class "uploaded"
                         (:form :method "post"
                                :action "upload"
                         (uploads-table-checkbox-form "Delete?"))))))
                 (:div :class "chooser"
                   (:form :method "post"
                          :enctype "multipart/form-data"
                          :action "upload"
                   (:p "File: "
                     (:input :type "file"
                             :name "file"))
                   (:p (:input :type "submit"))))))))))

(defvar max-number-of-sessions 10)

(defun set-next-session-id ()
  (if (= (1+ current-session-id) max-number-of-sessions)
      (setf current-session-id 0)
      (incf current-session-id)))

(defmacro ensure-valid-session (&body body)
  `(if (session-verify *request*)
       (progn ,@body)
       (with-title ("Invalid session")
	 (:h1 "Error")
	 (:p
"Something is wrong with your session.  Make sure that you have not
disabled cookies in your browser (at least, not for this web site).
Once you have enabled cookies, you may restart your session by going
to" (:a :href "start" "the start page") "."))))

;; /start
(define-xhtml-handler start-page ()
  (unless *session*
    (warn "First time visit -- we are starting a new session")
    (with-lock-held (session-id-lock)
      (let ((new-session (start-session))
	    (next-session-id (set-next-session-id)))
	(setf (gethash new-session hunchentoot-sessions->ids) 
	      next-session-id))))
  (with-title ("Reinhard's TeX Server")
    (:div :class "nav"
      (:ul
       (:li (:a :href "about" "about"))))
    (:h1 "Compile your TeX here")
    (:p 
"Welcome to Reinhard Kahle's TeX dungeon.")
    (:p
"Let's" (:a :href "upload" "get started") ".")))

(defun session-uploads ()
  (let ((session-id (gethash *session* hunchentoot-sessions->ids)))
    (gethash session-id session-uploads)))

;; /compile
(define-xhtml-handler compile-page ()
  (ensure-valid-session
   (with-title ("Compile your work")
     (let ((uploads (session-uploads)))
       (htm (:h1 "Your uploaded files")
	    (:div :class "uploaded")
	    (uploads-as-table)
	    (:p 
"If you wish to delete or submit updated versions of these files, go
to the" (:a :href "upload" "upload page") "." "Otherwise, select the
files on which you wish to operate, and choose the TeX program that
should process these files.")
	    
	    
	    
      
      

      
    

;;; Initialization and cleanup
(defun cleanup-sandboxes ()
  (when (directory-exists-p sandbox-root)
    (delete-directory-and-files sandbox-root))
  (dotimes (i 10)
    (let* ((i-as-str (format nil "~A" i))
	   (sandbox-dir (concatenate 'string
				     sandbox-root "/" i-as-str "/")))
      (ensure-directories-exist sandbox-dir))))

(defvar current-acceptor nil
  "The most recently created hunchentoot acceptor object.")

(defun startup (&optional (port 8080))
  (cleanup-sandboxes)
  (handler-case (progn
		  (setf current-acceptor (make-instance 'acceptor :port port))
		  (values t (start current-acceptor)))
    (usocket:address-in-use-error () 
      (values nil (format nil "Port ~A is already taken" port)))))

(defun shutdown ()
  (stop current-acceptor)
  (setf current-acceptor nil))
