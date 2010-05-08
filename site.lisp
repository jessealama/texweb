
(in-package :texserv)

(defvar texserv-root "/tmp/texserv/")

(defun append-to-texserv-root (path)
  (concatenate 'string
	       texserv-root
	       path))

(defvar texserv-bin-directory (append-to-texserv-root "bin/"))

;;; Hunchentoot configuration

(defvar maintainer-email-address "jesse.alama@gmail.com")

;; Dispatching

(defvar texserv-dispatch-table 
  `(,(create-prefix-dispatcher "/start" 'start-page)
     ,(create-prefix-dispatcher "/about" 'about-page)
     ,(create-prefix-dispatcher "/upload" 'upload-page)
     ,(create-prefix-dispatcher "/compile" 'compile-page)
     ,(create-prefix-dispatcher "/results" 'results-page)
     ,(create-prefix-dispatcher "/exit" 'exit-page)
     ,(create-prefix-dispatcher "/files" 'files-handler)))

(defun texserv-request-dispatcher (request)
  "The default request dispatcher which selects a request handler
based on a list of individual request dispatchers all of which can
either return a handler or neglect by returning NIL."
  (loop for dispatcher in texserv-dispatch-table
        for action = (funcall dispatcher request)
        when action return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

;; Logging

(defvar log-directory-root (append-to-texserv-root "logs/"))
(defvar message-log-pathname (concatenate 'string log-directory-root 
					          "messages"))
(defvar access-log-pathname (concatenate 'string log-directory-root "access"))
(setf *message-log-pathname* message-log-pathname)
(setf *access-log-pathname* access-log-pathname)
(setf *log-lisp-errors-p* t)
(setf *log-lisp-warnings-p* t)
(setf *log-lisp-backtraces-p* t)

;; (X)HTML output

(setq *attribute-quote-char* #\")
(setq *prologue*
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")

;;; Running programs

(defvar sandbox-root (append-to-texserv-root "sessions/")
  "The directory under which uploaded user data is stored.")

(defmacro with-xml-declaration (&body body)
  `(with-html-output-to-string (s)
     "<?xml version='1.1' encoding='UTF-8'?>"
     ,*prologue*
     (htm ,@body)))

(defmacro with-html (&body body)
  `(with-xml-declaration
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	,@body)))

(defmacro with-title (title &body body)
  `(with-html
     (:head (:title ,title))
     (:body ,@body)))

(defmacro define-xml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "application/xhtml+xml")
     (setf (header-out :server) nil) ; don't reveal the name of our web server
     ,@body))

(defmacro define-xhtml-handler (name (&rest args) &body body)
  `(defun ,name (,@args)
     (setf (content-type*) "text/html")
     (setf (header-out :server) nil) ; don't reveal the name of our web server
     ,@body))

;; /about
(define-xml-handler about-page ()
  (with-title "About this service"
    (:h1 "What this service provides")
    (:p "With the serivce provided by this site, you can upload your TeX work (TeX, LaTeX, ConTeXt, BibTeX, fonts, hyphenation patterns) to our server, compile the finished product, and download the results.")
    (:p "By default, Portable Document Format (PDF) output is generated. Optionally, one can specify PostScript (PS) and Device Independent (DVI) output as well.")
    (:h1 "What one needs to use this service")
    (:p "You need a modern web browser.  This web site adheres the the XHTML 1.1 document type. Although this document type specification is nearly ten years old, support for it might be irregular among older browsers. It is known to work with newer versions of Opera, Safari, Firefox, Camino, and Chrome.  There are all sorts of browsers with which this site works. There is one exception: Internet Explorer version 6 (and below). If you are using Internet Explorer, please use version 7 or 8. Version 6 of Internet Explorer (and below) is not supported.")
    (:h1 "How this service works")
    (:h1 "How your uploaded files are stored")
    (:p "Your uploaded work (TeX files, BibTeX files, etc.), and the results of compiling your work, will be stored on the server for at most one hour. After one hour, your files may be deleted at any time.")
    (:h1 "About security")
    (:p "There are two aspects of security that you should be aware of: your own security, and ours.  If you are impatient, here is a summary:")
    (:p (:b "USE THIS TOOL AT YOUR OWN RISK."))
    (:p "Concerning your security:")
    (:ul
     (:li "When uploading your work to this service, your work is not encrypted.")
     (:li "Just as your data is not encrypted during transmission from your machine to our server, your data is stored on our server without encryption.  We intend that different users of this web site are able to access only their own data, but we cannot promise that this is always the case; bugs or errors in TeX and friends, or in the programs that underlie this web site, could conceivably lead to your data being unintentionaly shared with other users whose intentions may not be noble.")
     (:li
"Your work resides on our server in an unencrypted form.  Your files may thus become accessed and stored by various services running on our server, such as automated backup scanners and search databases.  We intend to block your data from such tools, but we cannot promise that some tool, without our knowledge, would access your data.")
    (:li
"If your browser supports it, this site uses cookies.  If your browser doesn't support cookies, or if you have disabled support for cookies in your browser, this site will work.  But if cookies are enabled, our server will know that, and they will be used."))
    (:p "Concerning our own security:")
    (:ul
     (:li "To prevent abuse of our network resources, we limit the sizes of the files that can be uploaded onto our server.  The limit is " (:b "10 MB.") "Requests to transmit TeX files or any other file above this size will be denied.")
     (:li "To prevent abuse of our storage facilities, we limit the size of the files that can be generated.  The limit is " (:b "25 MB.") "If a file is generated whose size exceeds 25 MB, compilation will be aborted.")
    (:li "You can upload at most" (:b "25") "files.")
    (:li "We store data for at most" (:b "10") "users. If there are ten active sessions and a new user comes to this site, the data for the oldest session will be deleted."))
   (:p "If you are trying to carry out reasonable TeX work but these limitations prevent you from completing your task, please write to us and explain your situation.")))

(defvar sessions (make-hash-table)
  "A table of all the active sessions we are tracking.  Keys are names
of the session, i.e., the values of NAME variables in the session cookies.")

(defvar max-number-of-submitted-files 25
  "The number of files that a user is permitted to upload.")

(defvar max-file-size 10000000
  "The size of the largest file we will accept.")

(defun file-size (path)
  (if (file-exists-p path)
      (let ((stat (sb-posix:stat path)))
	(sb-posix:stat-size stat))
      (error "File ~A does not exist" path)))

(defvar session-uploads (make-hash-table)
  "A mapping from session names to lists of paths, saying which files
have been already uploaded for the session.")

(defvar session-handlers (make-hash-table)
  "A mapping from session IDs to lists of hunchentoot handlers, which serve files that were uploaded (or generated) in a session.")

(defvar current-session-id -1)
(defvar session-id-lock (make-lock "texserv"))
(defvar hunchentoot-sessions->ids (make-hash-table))

(defun list-longer-than (lst len)
  (if lst
      (list-longer-than (cdr lst) (1- len))
      (< len 0)))

(defun too-many-submitted? (session-id)
  (list-longer-than (gethash session-id session-uploads)
		    max-number-of-submitted-files))

(defun already-submitted? (session-id filename)
  "Has a file called FILENAME already been submitted in the session
whose ID is SESSION-ID?"
  (let ((previously-submitted (gethash session-id session-uploads)))
    (member filename previously-submitted :test #'string=)))

(defun session-id ()
  (gethash *session* hunchentoot-sessions->ids))

(defmacro cons-list-hash-value (key item table)
  `(setf (gethash ,key ,table)
	 (cons ,item (gethash ,key ,table))))

(defmacro incf-hash-value (key table &optional (initial-value 0))
  `(progn
     (when (null (gethash ,key ,table))      
       (setf (gethash ,key ,table) ,initial-value))
     (incf (gethash ,key ,table))))
  
(defun handle-file (post-parameter)
  (if post-parameter
      (if (listp post-parameter)
	  (destructuring-bind (path file-name content-type)
	      post-parameter
	    (declare (ignore content-type)) ;; don't know how to use this info
	    (if (session-verify *request*)
		(let ((session-id (session-id)))
		  (if session-id
		      (if (too-many-submitted? session-id)
			  :too-many-submitted-files
			  (if (string= file-name "")
			      :empty-file-name
			      (if (already-submitted? session-id file-name)
				  :duplicate-filename
				  (let ((size (file-size path)))
				    (if (< size max-file-size)
					(let ((new-path (file-in-session-dir session-id file-name)))
					; we need to sanitize this and/or
					; block bad inputs
					  (rename-file path new-path)
					  (cons-list-hash-value session-id
								file-name
								session-uploads)
					  ;; set up a new handler
					  (let ((new-handler (create-static-file-dispatcher-and-handler (format nil "/files/~A" file-name)
													(format nil "~A~A" (directory-for-session session-id)
														file-name))))
					    (push new-handler
						  (gethash session-id
							   session-handlers)))
					  (incf-hash-value session-id
							   sessions)
					  :ok)
				      :file-too-large)))))
		      :null-session-id))
		:verify-session-failure))
	  post-parameter)
      :null-post-parameter))

(defmethod session-cookie-name ((acceptor acceptor))
  (declare (ignore acceptor))
  "texserv")

(setq *rewrite-for-session-urls* nil)
(setq *session-max-time* (* 60 60)) ; one hour
(setq *use-remote-addr-for-sessions* t)
(setq *use-user-agent-for-sessions* t)

;; Garbage collection
(defun directory-for-session (session-id)
  (format nil "~A~A/" sandbox-root session-id))

(defun file-in-session-dir (session-id filename)
  (format nil "~A~A" (directory-for-session session-id)
	             filename))

(defmacro with-session-directory ((dir-var) &body body)
  (let ((session-id (gensym)))
    `(let ((,session-id (gethash *session* hunchentoot-sessions->ids)))
       (declare (ignorable ,session-id))
       (if ,session-id
	   (let ((,dir-var (directory-for-session ,session-id)))
	     ,@body)
	   (warn "Unable to get the session ID for this session; not doing anything")))))

(defun gc-session (session)
  (let ((session-id (gethash session hunchentoot-sessions->ids)))
    (setf (gethash session-id session-handlers) nil)
    (with-session-directory (sandbox-dir)
      (cond ((directory-exists-p sandbox-dir)
	     (delete-directory-and-files sandbox-dir)
	     (ensure-directories-exist sandbox-dir))
	    (t
	     (error 
	      "Error cleaning up session ~A, which maps to directory ~A:~%the directory does not exist!" session sandbox-dir))))))


(setf *session-removal-hook* #'gc-session)
(setq *session-gc-frequency* 10)

(defun maybe-parse-integer (str)
  (if str
      (parse-integer str)
      0))

(defmacro with-current-uploads ((uploads-var) &body body)
  (let ((session-id (gensym)))
    `(let* ((,session-id (gethash *session* hunchentoot-sessions->ids))
	    (,uploads-var (gethash ,session-id session-uploads)))
       ,@body)))

(defun uploads-table-checkbox-form (label-text)
  (with-current-uploads (uploads)
    (htm 
     (:table
      (:tr
       (:th "Filename")
       (:th label-text))
      (dolist (file uploads)
	(htm 
	 (:tr
	  (:td (str file))
	  (:td (:label :for file
		       label-text)
	       (:input :type "checkbox"
		       :id file
		       :name file)))))))))

(defun uploads-radio-form ()
  (with-current-uploads (uploads)
    (dolist (upload uploads)
      (htm (:p (str upload)
	       (:input :type "radio"
		       :name upload
		       :id upload))))))

(defun uploads-pulldown-menu ()
  (with-current-uploads (uploads)
    (htm 
     (:select :name "upload"
	      :size "1"
       (dolist (upload uploads)
	 (htm (:option (str upload))))))))

(defvar tex-and-friends 
  '("tex" "pdftex" "latex" "pdflatex" "bibtex" "context")
  "TeX and friends")

(defun tex-or-friend-radio-input (friend)
  (htm (:label :for friend (str friend))
       (:input :type "radio"
	       :name friend
	       :id friend)
       (:br)))

(defun choose-tex-and-friends-pulldown-menu ()
  (htm
   (:select :name "friend"
	    :size "1"
     (dolist (friend tex-and-friends)
       (htm (:option (str friend)))))))

(defun choose-tex-and-friends-radio-form (target)
  (htm
   (:form :action target
	  :method "post"
     (dolist (program tex-and-friends)
       (tex-or-friend-radio-input program))
     (htm
      (:p
       (:input :type "submit"
	       :name "Compile"))))))

(defvar upload-empty-file-name-message
  "The empty string cannot be the name of a file; please try again.")

(defvar duplicate-file-name-message
  "You are trying to upload a file whose name is identical to a file that you have already uploaded.  It's unclear how to proceed.  Did you do this by mistake?  If so, then no action is needed; continue to select additional files to upload, or proceed to compilation.  Are you trying to upload an updated version of the file you previously uploaded?  If so, first delete the old file with this name, then try uploading again.")

(defvar file-too-large-message
  (concatenate 'string
	       "The file you uploaded is too large (its size is greater than"
	       (format nil "~A" max-file-size) "bytes)."))

(defvar too-many-submitted-files-message
  (concatenate 'string
	       "You have already submitted" 
	       (format nil "~A" max-number-of-submitted-files)
	       "files; submitting  more is not permitted."))

(defvar null-session-id-message 
  "You are visiting this site without first obtaining a proper cookie. Please visit the start page to get one; from there you can follow a link to come back here.")

(defvar verify-session-failure-message
  "Your session with this site is in a strange state: either you are connecting now with a different web browser than the one you started this session with, or your IP address now differs from the one you started with.  Something is fishy; unable to proceed.")

;;; Handlers
(defmacro ensure-valid-session (&body body)
  `(cond ((session-verify *request*) ,@body)
	 (t
	  (setf (return-code*) 409)
	  (with-title "Invalid session"
	    (:h1 "Error")
	    (:p "Something is wrong with your session.  There are a few possible reasons:")
	    (:ul
	     (:li "Your session is too old.  Did you just now try to refresh the previous page more than" (fmt "~A" *session-max-time*) " seconds after your last activity with this site?  If so, please " (:a :href "start" "start over") " again.  If you uploaded files, you'll probably need to upload them again.  Sorry.")
	     (:li "You have disabled cookies in your browser.  Make sure that you have not disabled cookies in your browser (at least, not for this web site).  Once you have enabled cookies, you may restart your session by going to " (:a :href "start" "the start page") ".  If you uploaded any files before coming to this error page, you'll probably have to upload them again.  Sorry.")
	     (:li "You are trying to crack this web site or are probing this sytem by submitting nonsensical cookies.  Jackass."))))))


;; /start
(define-xml-handler upload-page ()
  ;; check to see if the incoming request is too big
  (ensure-valid-session
    (let* ((length-str (header-in* :content-length))
	   (length (maybe-parse-integer length-str)))
      (if (> length max-file-size)
	  (with-title "Too huge"
	    (setf (return-code*) 413)
	    (:h1 "Joker")
	    (:p "You submitted a file that is too large.  To ensure that this service is not abused, we limit uploaded files to " (fmt "~A" max-file-size) " bytes."))
	  (let ((just-getting-started nil)
		(session-id (gethash *session* hunchentoot-sessions->ids))
		(handle-result (handle-file (post-parameter "file"))))
	    (let ((uploads (gethash session-id session-uploads)))
	      (when (or (null uploads)
			(zerop (hash-table-count session-uploads))
			(null handle-result)) ;; not sure about this disjunction
		(setf just-getting-started t))
	      (with-title "Upload TeX data"
		(:div :class "messages"
		  (:p
		    (case handle-result
		      (:null-post-parameter
		       (if just-getting-started
			   (htm "Let's get started!")
			   (htm "Nothing submitted; please try again.")))
		      (:ok
		       (setf (return-code*) 201)
		       (htm "Upload more data?"))
		      (:empty-file-name
		       (setf (return-code*) 400)
		       (htm upload-empty-file-name-message))
		      (:duplicate-filename
		       (setf (return-code*) 400)
		       (htm duplicate-file-name-message))
		      (:file-too-large
		       (setf (return-code*) 413)
		       (htm file-too-large-message))
		      (:too-many-submitted-files
		       (setf (return-code*) 400)
		       (htm too-many-submitted-files-message))
		      (:null-session-id
		       (setf (return-code*) 400)
		       (htm null-session-id-message))
		      (:verify-session-failure
		       (setf (return-code*) 400)
		       (htm verify-session-failure-message))
		      (otherwise 
		       (setf (return-code*) 400)
		       (htm  "Uh oh, something is weird.  Received" 
			     (fmt "~A" handle-result) 
			     "from HANDLE-FILE.")))))
		(when (and session-id uploads)
		  (htm
		   (:div :class "uploaded"
			 (:form :method "post"
				:action "upload")
			 (uploads-table-checkbox-form "Delete?"))
		   (:p "If you're done uploading files, you may continue to " (:a :href "compile" "the compilation page") ".")))
		(:div :class "chooser"
		      (:form :method "post"
			     :enctype "multipart/form-data"
			     :action "upload"
		      (:p "File: "
			  (:input :type "file"
				  :name "file"))
		      (:p (:input :type "submit")))))))))))

(defvar max-number-of-sessions 10)

(defun set-next-session-id ()
  (if (= (1+ current-session-id) max-number-of-sessions)
      (setf current-session-id 0)
      (incf current-session-id)))

(setq *handle-http-errors-p* nil) ; Don't worry -- I got this

;; /start
(define-xml-handler start-page ()
  (unless *session*
    (warn "First time visit -- we are starting a new session")
    (with-lock-held (session-id-lock)
      (let ((new-session (start-session))
	    (next-session-id (set-next-session-id)))
	(setf (gethash new-session hunchentoot-sessions->ids) 
	      next-session-id))))
  (with-title "Reinhard's TeX Server"
    (:div :class "nav"
      (:ul
       (:li (:a :href "about" "about"))))
    (:h1 "Compile your TeX here")
    (:p "Welcome to Reinhard Kahle's TeX dungeon.")
    (:p "Let's " (:a :href "upload" "get started") ".")))

(defun session-uploads ()
  (let ((session-id (gethash *session* hunchentoot-sessions->ids)))
    (gethash session-id session-uploads)))

;; /compile
(define-xml-handler compile-page ()
  (ensure-valid-session
   (let ((uploads (session-uploads)))
     (if uploads
	 (with-title "Compile your work"
	   (:h1 "Your uploaded files")
	   (:p "If you wish to delete or submit updated versions of these files, go to the " (:a :href "upload" "upload page") ". Otherwise, select the files on which you wish to operate, and choose the TeX program that should process these files.")
	   (:form :action "results"
		  :method "post"
	     (uploads-pulldown-menu)
	     (choose-tex-and-friends-pulldown-menu)
	     (:input :type "submit")))
	 (with-title "Nothing to compile"
	   (setf (return-code*) 400)
	   (:p "You did not upload anything.  Please go to" (:a :href "upload" "the upload page") "to get upload files."))))))

(defvar texlive-binary-base
  "/usr/local/texlive/2009/bin/universal-darwin/"
  "The directory under which the TeX and friends binaries can be found.  It must end in a directory separator characer (e.g., /).")

(defvar programs-to-paths
  (mapcar #'(lambda (friend)
	      (cons friend 
		    (concatenate 'string 
				 texlive-binary-base
				 friend)))
	  tex-and-friends)
  "An association list that maps each member of TEX-AND-FRIENDS to
its corresponding binary.  It must be a full path.")

(defun friend-path (friend)
  (cdr (assoc friend programs-to-paths :test #'string=)))

(defvar run-tex-path "/Users/alama/Sites/tex/run-tex.sh")

(defun current-date-and-time-as-string ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0d--~2,'0d-~2,'0d-~2,'0d"
	    year month date hour minute second)))

(defun run-tex (friend work-dir file)
  "Execute the TeX friend FRIEND in directory WORK-DIR on file FILE.
FRIEND is a string whose value looks like \"tex\", \"pdflatex\",
\"bibtex\", etc.  FILE is understood relative to WORK-DIR.  Returns a
process object."
  (let ((date-time-str (current-date-and-time-as-string)))
    (let ((proc (sb-ext:run-program run-tex-path
				    (list friend work-dir file)
				    :input nil
				    :output (format nil "~A~A-output-~A" 
						    work-dir
						    friend
						    date-time-str)
				    :if-output-exists :supersede
				    :error :output
				    :environment nil
				    :search nil
				    :wait t)))
      (zerop (sb-ext:process-exit-code proc)))))

(defun compile-submission-with-friend (friend submission)
  "Given session ID (a number between 0 and 9), FRIEND (a member of
TEX-AND-FRIENDS), and filename SUBMISSION, run the program FRIEND
separately on the file associated with SUBMISSIOn.  SUBMISSION is to
be understood relative to the directory associated with the current
session.  Return T if the results were successful.  Otherwise, return
NIL.

Since the function runs a program that, generally, generates output
files, this function is not side effect-free.  The directory d
associated with the session id SESSION-ID will almost
certainly (provided that the program FRIEND is given well-formed
content in SUBMISSION) cause contain files after the execution of this
function as it did beforehand."
  (if (member friend tex-and-friends :test 'string=)
      (let ((friend-path (friend-path friend)))
	(if friend-path
	    (with-session-directory (session-dir)
	      (when (and session-dir (directory-exists-p session-dir))
		(run-tex friend session-dir submission)))))))

(defun list-session-directory (session-id)
  (mapcar #'file-namestring
	  (list-directory (directory-for-session session-id))))

(defun fetch-post-parameters (&rest params)
  (apply #'values
	 (mapcar #'post-parameter params)))

;; /results
(define-xml-handler results-page ()
  (ensure-valid-session
    (multiple-value-bind (friend upload)
	(fetch-post-parameters "friend" "upload")
      (cond ((and friend upload)
	     (compile-submission-with-friend friend upload)
	     (with-title "Here are your results"
	       (:h1 "The current listing of your directory")
	       (:ul
		(dolist (file (list-session-directory (session-id)))
		  (let ((file-uri (format nil "files/~A" file)))
		    (htm (:li (:a :href file-uri (str file)))))))
	       (:p "To download your work, simply follow one of the links to the newly generated files.")
	       (:p "If you would like to operate on more files, proceed to " (:a :href "compile" "the compile page") ".  The files that were just generated will be available to you as though you had uploaded them.")
	       (:p "If you would like to get a compressed copy of your work directory, choose the compression format and follow this link.")
	       (:p "If you are done, proceed to " (:a :href "exit" "the exit") ".  By exiting, you indicate that it is OK to delete your uploaded files and whatever intermediate files were generated during this session.")))
	    (t
	     (setf (return-code*) 400)
	     (with-title "Malformed request"
	       (:p "Your browser did not send a proper request to the server.  On the compilation page, you must specify both an uploaded file on which to operate, and a TeX program that will compile the specified uploaded file.  Your request omitted one of these pieces of information.  Please return to " (:a :href "compile" "the compile page") " to try again, or proceed to " (:a :href "exit" "the exit page") " to exit this service.  (By exiting, you are indicating that you would like the fies that you uploaded, if any, to be deleted, as well as any files that were generated during this session.)")))))))

;; /exit
(define-xml-handler exit-page ()
  (ensure-valid-session
    (let ((email-anchor (format nil "mailto:~A" maintainer-email-address)))
      (with-title "Exit"
	(:p "Thanks for using this service; we hope you were able to accomplish your TeX tasks with it.")
	(:p "Your uploaded files and generated files have been deleted from the server.")
	(:p "If you would like to start a new task by uploading and compiling new work, proceed to " (:a :href "start" "the start page") ".")
	(:p "If you would like to comment on the service, submit bug reports or request new features, please write to " (:a :href email-anchor (str maintainer-email-address)) ".  We appreciate your feedback.")))))

;;; Initialization and cleanup
(defun cleanup-sandboxes ()
  (when (directory-exists-p sandbox-root)
    (delete-directory-and-files sandbox-root))
  (dotimes (i 10)
    (let* ((i-as-str (format nil "~A" i))
	   (sandbox-dir (concatenate 'string
				     sandbox-root "/" i-as-str "/")))
      (ensure-directories-exist sandbox-dir))))

(defun create-empty-file (path)
  (if (file-exists-p path)
      (unless (zerop (file-size path))
	(error "There's already a file at ~A and it's non-empty!" path))
      (with-open-file (out path 
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists :supersede) ;; "should never happen"
	(declare (ignore out)))))

(defun cleanup-logs ()
  (when (directory-exists-p log-directory-root)
    (delete-directory-and-files log-directory-root))
  (ensure-directories-exist log-directory-root)
  (create-empty-file message-log-pathname)
  (create-empty-file access-log-pathname))

(defvar current-acceptor nil
  "The most recently created hunchentoot acceptor object.")

(defun startup (&optional (port 8080))
  (cleanup-sandboxes)
  (cleanup-logs)
  (handler-case (progn
		  (setf current-acceptor 
			(make-instance 
			 'acceptor 
			 :port port
			 :request-dispatcher 'texserv-request-dispatcher))
		  (values t (start current-acceptor)))
    (usocket:address-in-use-error ()
      (values nil (format nil "Port ~A is already taken" port)))))

(defun shutdown ()
  (stop current-acceptor)
  (setf current-acceptor nil))
