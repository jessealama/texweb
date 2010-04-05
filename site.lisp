(in-package :cl-user)

(require 'asdf)
(asdf:oos 'asdf:load-op :cl-who)
(asdf:oos 'asdf:load-op :hunchentoot)
(asdf:oos 'asdf:load-op :parenscript)

(defpackage tex-serv
  (:use :cl 
	:cl-who
	:hunchentoot
	:parenscript))

(in-package :tex-serv)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defvar *tmp-test-files* nil)

(defvar *tmp-test-directory*
    #p"/tmp/hunchentoot/test/")

(defun clean-tmp-dir ()
  (loop for (path . nil) in *tmp-test-files*
        when (probe-file path)
        do (ignore-errors (delete-file path)))
  (setq *tmp-test-files* nil))

(let ((counter 0))
  (defun handle-file (post-parameter)
    (when (and post-parameter
               (listp post-parameter))
      (destructuring-bind (path file-name content-type)
          post-parameter
        (let ((new-path (make-pathname :name (format nil "hunchentoot-test-~A"
                                                     (incf counter))
                                       :type nil
                                       :defaults *tmp-test-directory*)))
          ;; strip directory info sent by Windows browsers
          (when (search "Windows" (user-agent) :test 'char-equal)
            (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
          (rename-file path (ensure-directories-exist new-path))
          (push (list new-path file-name content-type) *tmp-test-files*))))))

(defun upload-test ()
  (let (post-parameter-p)
    (when (post-parameter "file1")
      (handle-file (post-parameter "file1"))
      (setq post-parameter-p t))
    (when (post-parameter "file2")
      (handle-file (post-parameter "file2"))
      (setq post-parameter-p t))
    (when (post-parameter "clean")
      (clean-tmp-dir)
      (setq post-parameter-p t))
    (when post-parameter-p
      ;; redirect so user can safely use 'Back' button
      (redirect (script-name*))))
  (no-cache)
  (with-html
    (:html
     (:head (:title "Hunchentoot file upload test"))
     (:body
      (:h2 "TeX file upload test")
      (:form :method :post :enctype "multipart/form-data"
       (:p "First file: "
        (:input :type :file
         :name "file1"))
       (:p "Second file: "
        (:input :type :file
         :name "file2"))
       (:p (:input :type :submit)))
      (when *tmp-test-files*
        (htm
         (:p
          (:table :border 1 :cellpadding 2 :cellspacing 0
           (:tr (:td :colspan 3 (:b "Uploaded files")))
           (loop for (path file-name nil) in *tmp-test-files*
                 for counter from 1
                 do (htm
                     (:tr (:td :align "right" (str counter))
                      (:td (:a :href (format nil "files/~A?path=~A"
                                             (url-encode file-name)
                                             (url-encode (namestring path)))
                            (esc file-name)))
                      (:td :align "right"
                       (str (ignore-errors
                              (with-open-file (in path)
                                (file-length in))))
                       "&nbsp;Bytes"))))))
         (:form :method :post
          (:p (:input :type :submit :name "clean" :value "Delete uploaded files")))))))))

(setq *dispatch-table*
      (nconc
       (list 'dispatch-easy-handlers
             (create-static-file-dispatcher-and-handler
              "/favicon.ico"
              (make-pathname :name "favicon" :type "ico" :version nil
                             :defaults *this-file*))
       (mapcar (lambda (args)
                 (apply 'create-prefix-dispatcher args))
                 ("/index.html" upload-test))
       (list 'default-dispatcher)))