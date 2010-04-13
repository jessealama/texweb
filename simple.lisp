(dolist (d '(cl-who hunchentoot))
  (asdf:oos 'asdf:load-op))

(defpackage :testserv
  (:use :cl
	:cl-who
	:hunchentoot)
  (:export :start))

(in-package :testserv)

(setq *dispatch-table*
      `(,(create-prefix-dispatcher "/test" 'test-page)
	,(create-prefix-dispatcher "/about" 'about-page)))

(setq *prologue*
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml11.dtd\">")

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

(define-xhtml-handler test-page ()
  (let ((name (parameter "name")))
    (if name
	(with-title ("Thank you!")
	   (:h1 "Thanks for giving me your name")
	   (:p (fmt "Hi, <b>~a</b>" name)))
	(with-title ("I need your input.")
	   (:form :action "/test"
		  :method "get"
		  (:p "Name: " (:input :type "text" :name "name")
		               (:input :type "submit" :name "submit")))))))

(define-xhtml-handler about-page ()
  (with-title ("Demo")
    (:h1 "Hunchentoot Demo")
    (:p "This is a very simple demonstration of the Hunchentoot webserver.")))
