
(in-package :cl-user)

(defpackage :texserv-asd
  (:use :cl :asdf))

(in-package :texserv-asd)

(defsystem texserv
  :name "texserv"
  :version "0.1"
  :author "Jesse Alama <jesse.alama@gmail.com>"
  :maintainer "Jesse Alama <jesse.alama@gmail.com>"
  :license "GPL"
  :description "A server for processing documents with TeX and friends"
  :long-description ""
  :components ((:file "packages")
	       (:file "site" :depends-on ("packages")))
  :depends-on (:cl-fad
	       :bordeaux-threads
	       :cl-who
	       :parenscript
	       :usocket
	       :hunchentoot))
  