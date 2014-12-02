;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2002-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      movitz.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Mon Oct  9 20:52:58 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: movitz.lisp,v 1.12 2007/03/13 20:40:10 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:movitz)

(defvar *image* nil "Holds the current image.")
(defvar *i* nil "The previous value of *image*")
(defvar *ii* nil "The previous value of *i*")

(define-symbol-macro *movitz-nil*
    (image-nil-object *image*))

(define-unsigned lu16 2 :little-endian)
(define-unsigned lu32 4 :little-endian)

(defconstant +code-vector-word-offset+ 2)
(defconstant +code-vector-transient-word+
  (ldb (byte 32 0) (- +code-vector-word-offset+)))

(defvar +movitz-multiple-values-limit+ 63)

(defvar *backquote-depth* 0)

(defvar *default-image-init-file*
  (asdf:system-relative-pathname :movitz "losp/los0" :type "lisp"))
(defvar *default-image-file*
  (asdf:system-relative-pathname :movitz "los0-image"))

(defvar *movitz-host-features* *features*
  "The *features* of the host implementation.")

(defmacro with-host-environment (options &body body)
  "Execute body in a `normal' host environment."
  (declare (ignore options))
  `(let ((*features* *movitz-host-features*))
     ,@body))

(defmacro print-unreadable-movitz-object ((object stream &rest key-args) &body body)
  "Just like print-unreadable-object, just adorn output so as to
make clear it's a Movitz object, with extra <..>"
  (let ((stream-var (gensym "unreadable-movitz-stream-")))
    `(let ((,stream-var ,stream))
       (print-unreadable-object (,object ,stream-var ,@key-args)
	 (write-char #\< ,stream-var)
	 ,@body
	 (write-char #\> ,stream-var)))))

(defmacro with-movitz-syntax (options &body body)
  (declare (ignore options))
  `(let ((*readtable* *movitz-readtable*))
     ,@body))

(defun qq-list (form level)
  (check-type form cons)
  (flet ((gen-list (&rest args) (apply #'list 'list args))
         (gen-append (&rest args) (apply #'list 'append args))
         (kwote (form) (list 'quote form)))
    (cons
     'append
     (loop for sub-form-head on form
           for sub-form = (and (consp sub-form-head)
                               (car sub-form-head))
           collect
           (if (atom sub-form)
               (kwote (list sub-form))
               (case (car sub-form)
                 (muerte::movitz-backquote
                  (gen-list
                   (gen-list (kwote 'muerte::movitz-backquote)
                             (un-backquote (cadr sub-form) (1+ level)))))
                 (unquote
                  (cond
                    ((zerop level)
                     (gen-list (cadr sub-form)))
                    ((and (listp (cadr sub-form))
                          (eq 'unquote-splicing (caadr sub-form))) 
                     (gen-append
                      (list 'mapcar
                            '(lambda (x) (list 'unquote x))
                            (cadr (cadr sub-form)))))
                    (t (gen-list
                        (gen-list
                         (kwote 'unquote)
                         (un-backquote (cadr sub-form) (1- level)))))))
                 (unquote-splicing
                  (if (zerop level)
                      (cadr sub-form)
                      (gen-list
                       (gen-list
                        (kwote 'unquote-splicing)
                        (un-backquote (cadr sub-form) (1- level))))))
                 (t (gen-list (un-backquote sub-form level)))))
           when (not (listp (cdr sub-form-head)))
             collect (kwote (cdr sub-form-head))))))

(defun un-backquote (form level)
  "Dont ask.."
  (declare (notinline un-backquote))
  (assert (not (minusp level)))
  (typecase form
    (null nil)
    (list (case (first form)
            (unquote (second form))
            (otherwise (qq-list form level))))
    (array (error "Array backquote not implemented."))
    (t (list 'quote form))))

(defmacro muerte::movitz-backquote (form)
  (un-backquote form 0))
