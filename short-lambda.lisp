;; short-lambda - a shorthand for defining lambda functions
;;
;; This module implements the reader macro #[...] which is used to
;; define short lambda functions that can be passed to functions such
;; as map, mapcar and maplist.
;;
;;  The following is an example usage:
;;
;;	* (map #[ + _ 1 ] '(1 2 3)
;;	(2 3 4)
;;
;; Notice the automatic definition of the variable _. This is the
;; argument that is passed to your function.
;;
;; The macro parameter can be used to pass the number of arguments
;; that you would like defined in the resulting function.  Variable
;; names follow the pattern of: _ (1st), __ (2nd), ___ (3rd), etc.
;; Notice that this can get a bit unwieldy, so it is not really
;; reccomended, but available if needed.  Here's an example:
;;
;;	* (mapcar #3[ + _ __ ___ ] '(1 2 3) '(1 2 3) '(1 2 3))
;;	(3 6 9)
;;
;; They can also be nested, like so:
;;
;;	* (mapcar #2[mapcar #[ + _ 2] (append (list _ _) (list _ __))]
;;		'(1 2 3) '(1 2 3))
;;	((3 3 3 3) (4 4 4 4) (5 5 5 5))
;;
;; Astute readers might notice the similarity to a syntax feature of
;; Arc.  I just thought it would be interesting to write.  Enjoy.
;;
;; Bugs:
;;
;; - ] characters prior to the end of the #[...] sequence will cause
;;   the reader to break ie. (#[map nil #[print _] "[abc]"] 0)
;;
;; Author: Burton Samograd <kruhft@gmail.com>
;; Date: May 26, 2011
;; License: 3 Clause BSD

(defpackage :short-lambda
  (:use :cl))
(in-package :short-lambda)

(defun |#[-reader| (stream subchar arg)
  (declare (ignore subchar))
  (let* ((sb (make-array 16
			 :element-type 'extended-char
			 :adjustable t
			 :fill-pointer 0)))
    (labels ((-> (c v) ; push c onto end of vector v
	       (vector-push-extend c v 16)))
      (map nil (lambda (c) (-> c sb)) "(lambda (")
      (if arg
	  (let ((var (make-array arg
				 :element-type 'extended-char
				 :adjustable t
				 :fill-pointer 0)))
	    (-> #\_ var)
	    (do ((i 0 (1+ i)))
		((= i arg))
	      (map nil (lambda (c) (-> c sb)) var)
	      (-> #\  sb)
	      (-> #\_ var)))
	  (-> #\_ sb))
      (-> #\) sb)
      (-> #\( sb)

      (let ((counter 1))
	(do ((c (read-char stream) (read-char stream)))
	    (nil)
	  (when (char= c #\#)
	    (if (char= (peek-char nil stream) #\[)
		(incf counter)))
	  (when (char= c #\])
	    (decf counter))
	  (when (= counter 0)
	    (return))
	  (-> c sb)))
      
      (-> #\) sb)
      (-> #\) sb))
    (read-from-string sb)))

(set-dispatch-macro-character #\# #\[ #'|#[-reader|)
