
(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :texserv-asd)
    (defpackage :texserv-asd
      (:use :cl :asdf))))

(in-package :texserv-asd)

(defsystem :cl-who-kryukov
  :version "0.11.1-kryukov"
  :serial t
  :components ((:file "cl-who/packages")
               (:file "cl-who/specials")
               (:file "cl-who/who")))

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
	       :cl-who-kryukov
	       :parenscript
	       :usocket
	       :hunchentoot-utils
	       :hunchentoot))
