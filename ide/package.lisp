;;; package.lisp

(defpackage movitz.ide
  (:use #:cl)
  (:export #:compile-movitz-file
           #:compile-defun
           #:dump-image
           #:movitz-disassemble
           #:movitz-disassemble-primitive
           #:movitz-disassemble-method
           #:movitz-arglist
           #:movitz-macroexpand))
