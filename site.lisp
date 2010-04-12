(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (d '(cl-who hunchentoot cl-fad))
    (asdf:oos 'asdf:load-op d)))

(defpackage :texserv
  (:use :cl
	:cl-who
	:cl-fad
	:hunchentoot))

(in-package :texserv)

(setq *dispatch-table*
      `(,(create-prefix-dispatcher "/main" 'main-page)
	,(create-prefix-dispatcher "/about" 'about-page)
	,(create-prefix-dispatcher "/start" 'start-page)))

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
     "<?xml version='1.0' encoding='UTF-8'?>"
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
		(let ((session-id (session-cookie-value *session*)))
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
						   (pathname-as-directory
						    (make-pathname 
						     :name session-id
						     :directory sandbox-root))))
					      (unless (directory-exists-p 
						       session-root)
						(ensure-directories-exist 
						 session-root))
					      (warn "session root is now ~A" session-root)
					      (let ((new-path 
						     (pathname-as-file
						      (make-pathname 
						       :name file-name
						       :directory session-root))))
					; we need to sanitize this input more
						(ensure-directories-exist new-path)
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

(setq *rewrite-for-session-urls* nil)
(setq *session-max-time* 3600) ; one hour
(setq *session-gc-frequency* 10)
(setq *use-remote-addr-for-sessions* t)
(setq *use-user-agent-for-sessions* t)
; (setf (header-out :server) "web server 0.2") ; don't reveal the name of our web server

(define-xhtml-handler start-page ()
  (let ((just-getting-started nil)
	(current-session (session-cookie-value *session*))
	(handle-result (handle-file (post-parameter "file"))))
    (let ((uploads (gethash current-session session-uploads)))
      (when (or (null uploads)
		(zerop (hash-table-count session-uploads))
		(null handle-result)) ;; not sure if this disjunction is good
	(setf just-getting-started t))
      (with-title ((format nil "Upload TeX data: ~A" handle-result))
	(:div :class "messages"
	  (:p
	    (case handle-result
	     (:null-post-parameter (if just-getting-started
				       (htm
"Let's get started!")
				       (htm
"You didn't submit anything; please try again.")))
	     (:ok (htm
"Upload more data?"))
	     (:empty-file-name (htm
"The empty string cannot be the name of a file; please try again."))
	     (:duplicate-filename (htm
"You are trying to upload a file whose name is identical to a file
that you have already uploaded.  It's unclear how to proceed.  Did you
do this by mistake?  If so, then no action is needed; continue to
select additional files to upload, or proceed to compilation.  Are you
trying to upload an updated version of the file you previously
uploaded?  If so, first delete the old file with this name, then
try uploading again."))
	     (:file-too-large (htm
"The file you uploaded is too large (its size is greater than"
(fmt "~A" max-file-size) "bytes)."))
             (:too-many-submitted-files (htm
"You have already submitted" (fmt "~A" max-number-of-submitted-files)
"files; submitting  more is not permitted."))
             (:null-session-id (htm
"You are visiting this site without first obtaining a proper cookie.
Please visit" (:a :href "main" "the main page") "to get one; from
there you can follow a link to come back here."))
             (:verify-session-failure (htm
"Your session with this site is in a strange state: either you are
connecting now with a different web browser than the one you started
this session with, or your IP address now differs from the one you
started with.  Something is fishy; unable to proceed."))
             (otherwise (htm "Uh oh, something is weird.  Received" (fmt "~A" handle-result) "from HANDLE-FILE.")))))
        (when (and current-session uploads)
          (htm
            (:div :class "uploaded"
              (:form :method "post"
                     :action "start"
              (:table
                (dolist (file uploads)
                  (htm 
		   (:tr
                    (:td (fmt "~A" file))
		    ; we might have to worry about the names of files
		    ; given to us: are they really acceptable values for
		    ; POST parameters?
		    (:td
		     (:label :for (fmt "~A" file)
			     "Delete?")
		     (:input :type "checkbox"
			     :id (fmt "~A" file)
			     :name (fmt "~A" file)))))))))))
        (:div :class "chooser"
          (:form :method "post"
                 :enctype "multipart/form-data"
                 :action "start"
	    (:p "File: "
	        (:input :type "file"
	                :name "file"))
	    (:p (:input :type "submit"))))))))

    
(define-xhtml-handler main-page ()
  (unless *session*
    (warn "First time visit -- we are starting a new session")
    (start-session))
  (with-title ("Reinhard's TeX Server")
    (:div :class "nav"
      (:ul
       (:li (:a :href "about" "about"))))
    (:h1 "Compile your TeX here")
    (:p 
"Welcome to Reinhard Kahle's TeX dungeon.")
    (:p
"Let's " (:a :href "start" "get started") ".")))
