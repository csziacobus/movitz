;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2003-2004, 
;;;;    Department of Computer Science, University of Tromso, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      variables.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Wed Nov  5 21:53:34 2003
;;;;                
;;;; $Id: variables.lisp,v 1.8 2004/09/15 10:22:59 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(require :muerte/basic-macros)
(provide :muerte/variables)

(in-package muerte)

(defconstant lambda-list-keywords
    '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

(defvar * nil)
(defvar ** nil)
(defvar *** nil)

(defvar / nil)
(defvar // nil)
(defvar /// nil)

(defvar + nil)
(defvar ++ nil)
(defvar +++ nil)

(defvar *read-base* 10)
(defvar *package* nil)

(defparameter *debugger-hook* nil)
(defvar *active-condition-handlers* nil)
(defvar *multiboot-data* nil)

(defvar internal-time-units-per-second)

(declaim (special *build-number*))
