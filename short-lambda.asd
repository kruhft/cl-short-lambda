;;; -*- Mode: Lisp -*-

(defpackage :short-lambda-system
  (:use :cl :asdf))
(in-package :short-lambda-system)

(defsystem :short-lambda
  :name "short-lambda"
  :author "Burton Samograd <burton.samograd@gmail.com>"
  :version "1.0"
  :maintainer "Burton Samograd <burton.samograd@gmail.com>"
  :license "GNU General Public License v3"
  :description "Short lambda's similar to arc."
  :serial t
  :components ((:file "short-lambda")))

