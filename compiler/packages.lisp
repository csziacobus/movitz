;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2003-2005, 
;;;;    Department of Computer Science, University of Tromsoe, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      packages.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Sat Nov 15 21:39:55 2003
;;;;                
;;;; $Id: packages.lisp,v 1.61 2008-04-27 19:20:06 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------


(defpackage muerte.mop
  (:use)
  (:export reader-method-class
	   remove-direct-method
	   class-direct-subclasses
	   method-lambda-list
	   compute-default-initargs
	   add-direct-method
	   standard-instance-access
	   compute-discriminating-function
	   specializer-direct-methods
	   standard-writer-method
	   standard-accessor-method
	   forward-referenced-class
	   class-precedence-list
	   funcallable-standard-class
	   eql-specializer-object
	   generic-function-name
	   compute-slots
	   remove-dependent
	   slot-makunbound-using-class
	   generic-function-methods
	   compute-effective-slot-definition
	   validate-superclass
	   class-direct-default-initargs
	   add-dependent
	   effective-slot-definition
	   method-function
	   direct-slot-definition-class
	   slot-definition
	   slot-definition-writers
	   slot-definition-allocation
	   generic-function-declarations
	   slot-definition-initform
	   class-direct-superclasses
	   class-default-initargs
	   method-generic-function
	   find-method-combination
	   make-method-lambda
	   ensure-class
	   generic-function-argument-precedence-order
	   direct-slot-definition
	   generic-function-lambda-list
	   ensure-generic-function-using-class
	   standard-slot-definition
	   add-direct-subclass
	   slot-value-using-class
	   writer-method-class
	   standard-effective-slot-definition
	   class-prototype
	   slot-definition-initargs
	   metaobject
	   eql-specializer
	   slot-definition-readers
	   set-funcallable-instance-function
	   compute-class-precedence-list
	   remove-direct-subclass
	   standard-direct-slot-definition
	   class-finalized-p
	   slot-definition-initfunction
	   funcallable-standard-instance-access
	   standard-reader-method
	   accessor-method-slot-definition
	   ensure-class-using-class
	   specializer-direct-generic-functions
	   class-direct-slots
	   slot-definition-name
	   funcallable-standard-object
	   update-dependent
	   compute-effective-method
	   slot-boundp-using-class
	   class-slots
	   specializer
	   finalize-inheritance
	   extract-lambda-list
	   map-dependents
	   generic-function-method-class
	   intern-eql-specializer
	   effective-slot-definition-class
	   compute-applicable-methods-using-classes
	   generic-function-method-combination
	   slot-definition-type
	   slot-definition-location
	   method-specializers
	   extract-specializer-names))

(defpackage muerte.common-lisp
  (:nicknames muerte.cl)
  (:use )
  (:import-from common-lisp cl:nil)
  (:export &allow-other-keys
	   &aux
	   &body
	   &environment
	   &key
	   &optional
	   &rest
	   &whole
	   *
	   **
	   ***
	   *break-on-signals*
	   *compile-file-pathname*
	   *compile-file-truename*
	   *compile-print*
	   *compile-verbose*
	   *debug-io*
	   *debugger-hook*
	   *default-pathname-defaults*
	   *error-output*
	   *features*
	   *gensym-counter*
	   *load-pathname*
	   *load-print*
	   *load-truename*
	   *load-verbose*
	   *macroexpand-hook*
	   *modules*
	   *package*
	   *print-array*
	   *print-base*
	   *print-case*
	   *print-circle*
	   *print-escape*
	   *print-gensym*
	   *print-length*
	   *print-level*
	   *print-lines*
	   *print-miser-width*
	   *print-pprint-dispatch*
	   *print-pretty*
	   *print-radix*
	   *print-readably*
	   *print-right-margin*
	   *query-io*
	   *random-state*
	   *read-base*
	   *read-default-float-format*
	   *read-eval*
	   *read-suppress*
	   *readtable*
	   *standard-input*
	   *standard-output*
	   *terminal-io*
	   *trace-output*
	   +
	   ++
	   +++
	   -
	   /
	   //
	   ///
	   /=
	   1+
	   1-
	   <
	   <=
	   =
	   >
	   >=
	   abort
	   abs
	   acons
	   acos
	   acosh
	   add-method
	   adjoin
	   adjust-array
	   adjustable-array-p
	   allocate-instance
	   alpha-char-p
	   alphanumericp
	   and
	   append
	   apply
	   apropos
	   apropos-list
	   aref
	   arithmetic-error
	   arithmetic-error-operands
	   arithmetic-error-operation
	   array
	   array-dimension
	   array-dimension-limit
	   array-dimensions
	   array-displacement
	   array-element-type
	   array-has-fill-pointer-p
	   array-in-bounds-p
	   array-rank
	   array-rank-limit
	   array-row-major-index
	   array-total-size
	   array-total-size-limit
	   arrayp
	   ash
	   asin
	   asinh
	   assert
	   assoc
	   assoc-if
	   assoc-if-not
	   atan
	   atanh
	   atom
	   base-char
	   base-string
	   bignum
	   bit
	   bit-and
	   bit-andc1
	   bit-andc2
	   bit-eqv
	   bit-ior
	   bit-nand
	   bit-nor
	   bit-not
	   bit-orc1
	   bit-orc2
	   bit-vector
	   bit-vector-p
	   bit-xor
	   block
	   boole
	   boole-1
	   boole-2
	   boole-and
	   boole-andc1
	   boole-andc2
	   boole-c1
	   boole-c2
	   boole-clr
	   boole-eqv
	   boole-ior
	   boole-nand
	   boole-nor
	   boole-orc1
	   boole-orc2
	   boole-set
	   boole-xor
	   boolean
	   both-case-p
	   boundp
	   break
	   broadcast-stream
	   broadcast-stream-streams
	   built-in-class
	   butlast
	   byte
	   byte-position
	   byte-size
	   caaaar
	   caaadr
	   caaar
	   caadar
	   caaddr
	   caadr
	   caar
	   cadaar
	   cadadr
	   cadar
	   caddar
	   cadddr
	   caddr
	   cadr
	   call-arguments-limit
	   call-method
	   call-next-method
	   car
	   case
	   catch
	   ccase
	   cdaaar
	   cdaadr
	   cdaar
	   cdadar
	   cdaddr
	   cdadr
	   cdar
	   cddaar
	   cddadr
	   cddar
	   cdddar
	   cddddr
	   cdddr
	   cddr
	   cdr
	   ceiling
	   cell-error
	   cell-error-name
	   cerror
	   change-class
	   char
	   char-code
	   char-code-limit
	   char-downcase
	   char-equal
	   char-greaterp
	   char-int
	   char-lessp
	   char-name
	   char-not-equal
	   char-not-greaterp
	   char-not-lessp
	   char-upcase
	   char/=
	   char<
	   char<=
	   char=
	   char>
	   char>=
	   character
	   characterp
	   check-type
	   cis
	   class
	   class-name
	   class-of
	   clear-input
	   clear-output
	   close
	   clrhash
	   code-char
	   coerce
	   compilation-speed
	   compile
	   compile-file
	   compile-file-pathname
	   compiled-function
	   compiled-function-p
	   compiler-macro
	   compiler-macro-function
	   complement
	   complex
	   complexp
	   compute-applicable-methods
	   compute-restarts
	   concatenate
	   concatenated-stream
	   concatenated-stream-streams
	   cond
	   condition
	   conjugate
	   cons
	   consp
	   constantly
	   constantp
	   continue
	   control-error
	   copy-alist
	   copy-list
	   copy-pprint-dispatch
	   copy-readtable
	   copy-seq
	   copy-structure
	   copy-symbol
	   copy-tree
	   cos
	   cosh
	   count
	   count-if
	   count-if-not
	   ctypecase
	   debug
	   decf
	   declaim
	   declaration
	   declare
	   decode-float
	   decode-universal-time
	   defclass
	   defconstant
	   defgeneric
	   define-compiler-macro
	   define-condition
	   define-method-combination
	   define-modify-macro
	   define-setf-expander
	   define-symbol-macro
	   defmacro
	   defmethod
	   defpackage
	   defparameter
	   defsetf
	   defstruct
	   deftype
	   defun
	   defvar
	   delete
	   delete-duplicates
	   delete-file
	   delete-if
	   delete-if-not
	   delete-package
	   denominator
	   deposit-field
	   describe
	   describe-object
	   destructuring-bind
	   digit-char
	   digit-char-p
	   directory
	   directory-namestring
	   disassemble
	   division-by-zero
	   do
	   do*
	   do-all-symbols
	   do-external-symbols
	   do-symbols
	   documentation
	   dolist
	   dotimes
	   double-float
	   double-float-epsilon
	   double-float-negative-epsilon
	   dpb
	   dribble
	   dynamic-extent
	   ecase
	   echo-stream
	   echo-stream-input-stream
	   echo-stream-output-stream
	   ed
	   eighth
	   elt
	   encode-universal-time
	   end-of-file
	   endp
	   enough-namestring
	   ensure-directories-exist
	   ensure-generic-function
	   eq
	   eql
	   equal
	   equalp
	   error
	   etypecase
	   eval
	   eval-when
	   evenp
	   every
	   exp
	   export
	   expt
	   extended-char
	   fboundp
	   fceiling
	   fdefinition
	   ffloor
	   fifth
	   file-author
	   file-error
	   file-error-pathname
	   file-length
	   file-namestring
	   file-position
	   file-stream
	   file-string-length
	   file-write-date
	   fill
	   fill-pointer
	   find
	   find-all-symbols
	   find-class
	   find-if
	   find-if-not
	   find-method
	   find-package
	   find-restart
	   find-symbol
	   finish-output
	   first
	   fixnum
	   flet
	   float
	   float-digits
	   float-precision
	   float-radix
	   float-sign
	   floating-point-inexact
	   floating-point-invalid-operation
	   floating-point-overflow
	   floating-point-underflow
	   floatp
	   floor
	   fmakunbound
	   force-output
	   format
	   formatter
	   fourth
	   fresh-line
	   fround
	   ftruncate
	   ftype
	   funcall
	   function
	   function-keywords
	   function-lambda-expression
	   functionp
	   gcd
	   generic-function
	   gensym
	   gentemp
	   get
	   get-decoded-time
	   get-dispatch-macro-character
	   get-internal-real-time
	   get-internal-run-time
	   get-macro-character
	   get-output-stream-string
	   get-properties
	   get-setf-expansion
	   get-universal-time
	   getf
	   gethash
	   go
	   graphic-char-p
	   handler-bind
	   handler-case
	   hash-table
	   hash-table-count
	   hash-table-p
	   hash-table-rehash-size
	   hash-table-rehash-threshold
	   hash-table-size
	   hash-table-test
	   host-namestring
	   identity
	   if
	   ignorable
	   ignore
	   ignore-errors
	   imagpart
	   import
	   in-package
	   incf
	   initialize-instance
	   inline
	   input-stream-p
	   inspect
	   integer
	   integer-decode-float
	   integer-length
	   integerp
	   interactive-stream-p
	   intern
	   internal-time-units-per-second
	   intersection
	   invalid-method-error
	   invoke-debugger
	   invoke-restart
	   invoke-restart-interactively
	   isqrt
	   keyword
	   keywordp
	   labels
	   lambda
	   lambda-list-keywords
	   lambda-parameters-limit
	   last
	   lcm
	   ldb
	   ldb-test
	   ldiff
	   least-negative-double-float
	   least-negative-long-float
	   least-negative-normalized-double-float
	   least-negative-normalized-long-float
	   least-negative-normalized-short-float
	   least-negative-normalized-single-float
	   least-negative-short-float
	   least-negative-single-float
	   least-positive-double-float
	   least-positive-long-float
	   least-positive-normalized-double-float
	   least-positive-normalized-long-float
	   least-positive-normalized-short-float
	   least-positive-normalized-single-float
	   least-positive-short-float
	   least-positive-single-float
	   length
	   let
	   let*
	   lisp-implementation-type
	   lisp-implementation-version
	   list
	   list*
	   list-all-packages
	   list-length
	   listen
	   listp
	   load
	   load-logical-pathname-translations
	   load-time-value
	   locally
	   log
	   logand
	   logandc1
	   logandc2
	   logbitp
	   logcount
	   logeqv
	   logical-pathname
	   logical-pathname-translations
	   logior
	   lognand
	   lognor
	   lognot
	   logorc1
	   logorc2
	   logtest
	   logxor
	   long-float
	   long-float-epsilon
	   long-float-negative-epsilon
	   long-site-name
	   loop
	   loop-finish
	   lower-case-p
	   machine-instance
	   machine-type
	   machine-version
	   macro-function
	   macroexpand
	   macroexpand-1
	   macrolet
	   make-array
	   make-broadcast-stream
	   make-concatenated-stream
	   make-condition
	   make-dispatch-macro-character
	   make-echo-stream
	   make-hash-table
	   make-instance
	   make-instances-obsolete
	   make-list
	   make-load-form
	   make-load-form-saving-slots
	   make-method
	   make-package
	   make-pathname
	   make-random-state
	   make-sequence
	   make-string
	   make-string-input-stream
	   make-string-output-stream
	   make-symbol
	   make-synonym-stream
	   make-two-way-stream
	   makunbound
	   map
	   map-into
	   mapc
	   mapcan
	   mapcar
	   mapcon
	   maphash
	   mapl
	   maplist
	   mask-field
	   max
	   member
	   member-if
	   member-if-not
	   merge
	   merge-pathnames
	   method
	   method-combination
	   method-combination-error
	   method-qualifiers
	   min
	   minusp
	   mismatch
	   mod
	   most-negative-double-float
	   most-negative-fixnum
	   most-negative-long-float
	   most-negative-short-float
	   most-negative-single-float
	   most-positive-double-float
	   most-positive-fixnum
	   most-positive-long-float
	   most-positive-short-float
	   most-positive-single-float
	   muffle-warning
	   multiple-value-bind
	   multiple-value-call
	   multiple-value-list
	   multiple-value-prog1
	   multiple-value-setq
	   multiple-values-limit
	   name-char
	   namestring
	   nbutlast
	   nconc
	   next-method-p
	   nil
	   nintersection
	   ninth
	   no-applicable-method
	   no-next-method
	   not
	   notany
	   notevery
	   notinline
	   nreconc
	   nreverse
	   nset-difference
	   nset-exclusive-or
	   nstring-capitalize
	   nstring-downcase
	   nstring-upcase
	   nsublis
	   nsubst
	   nsubst-if
	   nsubst-if-not
	   nsubstitute
	   nsubstitute-if
	   nsubstitute-if-not
	   nth
	   nth-value
	   nthcdr
	   null
	   number
	   numberp
	   numerator
	   nunion
	   oddp
	   open
	   open-stream-p
	   optimize
	   or
	   otherwise
	   output-stream-p
	   package
	   package-error
	   package-error-package
	   package-name
	   package-nicknames
	   package-shadowing-symbols
	   package-use-list
	   package-used-by-list
	   packagep
	   pairlis
	   parse-error
	   parse-integer
	   parse-namestring
	   pathname
	   pathname-device
	   pathname-directory
	   pathname-host
	   pathname-match-p
	   pathname-name
	   pathname-type
	   pathname-version
	   pathnamep
	   peek-char
	   phase
	   pi
	   plusp
	   pop
	   position
	   position-if
	   position-if-not
	   pprint
	   pprint-dispatch
	   pprint-exit-if-list-exhausted
	   pprint-fill
	   pprint-indent
	   pprint-linear
	   pprint-logical-block
	   pprint-newline
	   pprint-pop
	   pprint-tab
	   pprint-tabular
	   prin1
	   prin1-to-string
	   princ
	   princ-to-string
	   print
	   print-not-readable
	   print-not-readable-object
	   print-object
	   print-unreadable-object
	   probe-file
	   proclaim
	   prog
	   prog*
	   prog1
	   prog2
	   progn
	   program-error
	   progv
	   provide
	   psetf
	   psetq
	   push
	   pushnew
	   quote
	   random
	   random-state
	   random-state-p
	   rassoc
	   rassoc-if
	   rassoc-if-not
	   ratio
	   rational
	   rationalize
	   rationalp
	   read
	   read-byte
	   read-char
	   read-char-no-hang
	   read-delimited-list
	   read-from-string
	   read-line
	   read-preserving-whitespace
	   read-sequence
	   reader-error
	   readtable
	   readtable-case
	   readtablep
	   real
	   realp
	   realpart
	   reduce
	   reinitialize-instance
	   rem
	   remf
	   remhash
	   remove
	   remove-duplicates
	   remove-if
	   remove-if-not
	   remove-method
	   remprop
	   rename-file
	   rename-package
	   replace
	   require
	   rest
	   restart
	   restart-bind
	   restart-case
	   restart-name
	   return
	   return-from
	   revappend
	   reverse
	   room
	   rotatef
	   round
	   row-major-aref
	   rplaca
	   rplacd
	   safety
	   satisfies
	   sbit
	   scale-float
	   schar
	   search
	   second
	   sequence
	   serious-condition
	   set
	   set-difference
	   set-dispatch-macro-character
	   set-exclusive-or
	   set-macro-character
	   set-pprint-dispatch
	   set-syntax-from-char
	   setf
	   setq
	   seventh
	   shadow
	   shadowing-import
	   shared-initialize
	   shiftf
	   short-float
	   short-float-epsilon
	   short-float-negative-epsilon
	   short-site-name
	   signal
	   signed-byte
	   signum
	   simple-array
	   simple-base-string
	   simple-bit-vector
	   simple-bit-vector-p
	   simple-condition
	   simple-condition-format-arguments
	   simple-condition-format-control
	   simple-error
	   simple-string
	   simple-string-p
	   simple-type-error
	   simple-vector
	   simple-vector-p
	   simple-warning
	   sin
	   single-float
	   single-float-epsilon
	   single-float-negative-epsilon
	   sinh
	   sixth
	   sleep
	   slot-boundp
	   slot-exists-p
	   slot-makunbound
	   slot-missing
	   slot-unbound
	   slot-value
	   software-type
	   software-version
	   some
	   sort
	   space
	   special
	   special-operator-p
	   speed
	   sqrt
	   stable-sort
	   standard
	   standard-char
	   standard-char-p
	   standard-class
	   standard-generic-function
	   standard-method
	   standard-object
	   step
	   storage-condition
	   store-value
	   stream
	   stream-element-type
	   stream-error
	   stream-error-stream
	   stream-external-format
	   streamp
	   string
	   string-capitalize
	   string-downcase
	   string-equal
	   string-greaterp
	   string-left-trim
	   string-lessp
	   string-not-equal
	   string-not-greaterp
	   string-not-lessp
	   string-right-trim
	   string-stream
	   string-trim
	   string-upcase
	   string/=
	   string<
	   string<=
	   string=
	   string>
	   string>=
	   stringp
	   structure
	   structure-class
	   structure-object
	   style-warning
	   sublis
	   subseq
	   subsetp
	   subst
	   subst-if
	   subst-if-not
	   substitute
	   substitute-if
	   substitute-if-not
	   subtypep
	   svref
	   sxhash
	   symbol
	   symbol-function
	   symbol-macrolet
	   symbol-name
	   symbol-package
	   symbol-plist
	   symbol-value
	   symbolp
	   synonym-stream
	   synonym-stream-symbol
	   t
	   tagbody
	   tailp
	   tan
	   tanh
	   tenth
	   terpri
	   the
	   third
	   throw
	   time
	   trace
	   translate-logical-pathname
	   translate-pathname
	   tree-equal
	   truename
	   truncate
	   two-way-stream
	   two-way-stream-input-stream
	   two-way-stream-output-stream
	   type
	   type-error
	   type-error-datum
	   type-error-expected-type
	   type-of
	   typecase
	   typep
	   unbound-slot
	   unbound-slot-instance
	   unbound-variable
	   undefined-function
	   unexport
	   unintern
	   union
	   unless
	   unread-char
	   unsigned-byte
	   untrace
	   unuse-package
	   unwind-protect
	   update-instance-for-different-class
	   update-instance-for-redefined-class
	   upgraded-array-element-type
	   upgraded-complex-part-type
	   upper-case-p
	   use-package
	   use-value
	   user-homedir-pathname
	   values
	   values-list
	   variable
	   vector
	   vector-pop
	   vector-push
	   vector-push-extend
	   vectorp
	   warn
	   warning
	   when
	   wild-pathname-p
	   with-accessors
	   with-compilation-unit
	   with-condition-restarts
	   with-hash-table-iterator
	   with-input-from-string
	   with-open-file
	   with-open-stream
	   with-output-to-string
	   with-package-iterator
	   with-simple-restart
	   with-slots
	   with-standard-io-syntax
	   write
	   write-byte
	   write-char
	   write-line
	   write-sequence
	   write-string
	   write-to-string
	   y-or-n-p
	   yes-or-no-p
	   zerop))

(defpackage muerte.common-lisp-user
  (:nicknames muerte.cl-user)
  (:use muerte.common-lisp))


(defpackage muerte
  (:use muerte.mop muerte.common-lisp)
  (:import-from common-lisp cl:nil)
  (:shadow get-setf-expansion)
  (:export #:translate-program
	   #:decode-macro-lambda-list
	   #:with-inline-assembly
	   #:make-named-function
	   #:without-function-prelude
	   #:numargs-case
	   #:simple-read-from-string
	   #:read-key
	   #:fixnump
	   #:newline
	   #:check-the
	   #:index
	   #:make-stack-vector
	   #:stack-vector
	   
	   #:defmacro/cross-compilation
	   
	   #:*print-safely*
	   
	   #:*debugger-function*
	   #:*debugger-condition*
	   #:*backtrace-conflate-names*
	   #:*backtrace-do-conflate*
	   #:*backtrace-max-frames*
	   #:*backtrace-max-args*
	   #:*backtrace-on-error*
	   #:*backtrace-stack-frame-barrier*
	   #:*backtrace-do-fresh-lines*
	   #:*backtrace-be-spartan-p*
	   #:*backtrace-print-length*
	   #:*backtrace-print-level*
	   #:backtrace

	   #:dit-frame-ref
	   #:stack-frame-ref
	   #:stack-frame-uplink
	   #:stack-frame-funobj
	   #:check-stack-limit
	   #:current-stack-frame
	   #:interrupt-default-handler
	   #:exception-handler

	   #:*build-number*
	   #:*error-no-condition-for-debugger*
	   #:formatted-error

	   #:package-object-use-list
	   #:package-object-internal-symbols
	   #:package-object-external-symbols

	   #:map-lisp-vals
	   #:map-header-vals
	   #:map-stack-vector

	   #:%memory-map%
	   #:%memory-map-roots%

	   #:%word-offset
	   #:%run-time-context-slot
	   #:shallow-copy
	   
	   #:%symbol-global-value
	   #:define-global-variable
	   
	   #:movitz-type-slot-offset
	   
	   #:object-location
	   #:object-tag
	   #:location-in-object-p
	   #:location-physical-offset
	   #:define-compile-time-variable
	   #:define-primitive-function
	   #:without-gc
	   #:with-stack-check
	   #:with-symbol-mutex
	   #:spin-wait-pause
	   #:char-whitespace-p
	   #:wrong-argument-count
	   #:throw-error
	   #:*debugger-function*
	   #:*debugger-invoked-stack-frame*
	   #:*debugger-condition*
	   #:*debugger-dynamic-context*
	   #:pprint-clumps
	   #:do-trace
	   #:do-untrace
	   #:malloc-initialize
	   #:clos-bootstrap
	   #:*forward-generic-function*
	   #:halt-cpu
	   
	   #:find-restart-by-index
	   #:find-restart-from-context
	   #:map-active-restarts
	   #:with-basic-restart
	   
	   #:dynamic-variable-install
	   #:dynamic-variable-uninstall

	   #:code-vector
	   #:pointer
	   #:basic-restart
	   #:run-time-context
	   #:run-time-context-class
	   #:current-run-time-context
	   
	   make-funobj
	   funobj-type
	   funobj-code-vector
	   funobj-code-vector%1op
	   funobj-code-vector%2op
	   funobj-code-vector%3op
	   funobj-lambda-list
	   funobj-name
	   funobj-num-constants
	   funobj-num-jumpers
	   funobj-constant-ref
	   funobj-debug-info
	   install-function

	   simple-stream
	   null-simple-stream
	   with-stream-class sm
	   device-open
	   device-close
	   device-read
	   device-write
	   device-file-position
	   device-file-length
	   device-buffer-length
	   device-clear-output
	   device-clear-input
	   device-finish-record
	   
	   #:unfold-circular-list
	   #:translate-program
	   #:decode-macro-lambda-list
	   #:decode-optional-formal
	   #:decode-keyword-formal
	   #:parse-body
           #:parse-macro-lambda-list
	   #:compute-function-block-name
	   #:movitz-macroexpand
	   #:movitz-macroexpand-1
	   #:decode-optional-formal
	   #:decode-keyword-formal
	   
	   #:movitz-accessor
	   #:%object-lispval
	   #:%lispval-object
	   #:objects-equalp
	   #:&edx
	   
	   #:memref
	   #:memref-int
	   #:memrange

	   #:io-port
	   #:io-register8
	   #:io-register8x2
	   #:io-delay
	   #:io-port-read-sequence
	   #:io-port-write-sequence
	   #:%io-port-read-succession
	   #:%io-port-write-succession
	   #:with-io-register-syntax
	   #:with-register-syntax
	   #:without-interrupts
	   #:cpu-id
	   #:cpu-486-class-p
	   #:cpu-586-class-p
	   #:cpu-signature
	   #:cpu-featurep
	   #:find-cpu-features
	   #:*cpu-features*
	   #:*gc-hooks*
	   #:write-cpu-vendor-string
	   #:read-time-stamp-counter
	   #:clear-time-stamp-counter
	   #:eflags
	   #:decode-eflags
	   #:load-idt
	   #:segment-register
	   #:segment-descriptor
	   #:segment-descriptor-base-location
	   #:segment-descriptor-limit
	   #:segment-descriptor-type-s-dpl-p
	   #:segment-descriptor-avl-x-db-g
	   #:global-segment-descriptor-table
	   #:control-register-lo12
	   #:control-register-hi20
	   #:ensure-data-vector
	   #:vector-read
	   #:vector-read-more-p
	   ))

(defpackage #:movitz
  (:use :common-lisp :binary-types)
  (:export #:create-image
	   #:dump-image

	   #:class-object-offset
	   #:other-type-byte
	   #:+other-type-offset+
	   #:parse-docstring-and-declarations
	   #:global-constant-offset
	   #:tag #:tag-name
	   #:movitz-print
	   #:stream-image
	   #:with-procfs-image
	   #:*image* #:*i*
	   #:*default-image-file*
	   #:*default-image-init-file*
	   #:movitz-constantp
	   #:movitz-eval
	   #:movitz-subtypep

	   #:movitz-compile-file
           #:movitz-compile-stream
           #:movitz-disassemble
           #:movitz-disassemble-method
	   
	   #:movitz-std-instance
	   #:movitz-struct
	   #:type
	   #:movitz-funobj
	   #:funobj-type
	   #:code-vector
	   #:code-vector%1op
	   #:code-vector%2op
	   #:code-vector%3op
	   #:code-vector-word
	   #:lu32

	   #:+movitz-most-positive-fixnum+
	   #:+movitz-most-negative-fixnum+
	   #:+movitz-fixnum-factor+
	   #:+movitz-fixnum-bits+
	   #:+movitz-fixnum-shift+
	   #:+movitz-fixnum-zmask+
	   #:+scan-skip-word+
	   #:+code-vector-word-offset+
	   #:+code-vector-transient-word+
	   
	   #:movitz-object-browser-properties
	   #:movitz-heap-object
	   
	   #:movitz-basic-vector
	   #:movitz-vector-num-elements
	   #:movitz-vector-element-type
	   #:movitz-vector-symbolic-data
	   #:basic-vector-type-tag
	   #:vector-type-tag
	   
	   #:movitz-symbol
	   #:movitz-string
	   #:movitz-bignum

	   #:movitz-character
	   #:movitz-char

	   #:movitz-struct
	   #:movitz-struct-length	
	   #:movitz-struct-slot-values
   
	   #:movitz-intern
	   #:movitz-read
	   #:movitz-read-and-intern
	   #:movitz-word
	   #:word

	   #:*warn-function-change-p*
	   #:*compiler-do-optimize*
	   #:*compiler-use-cmov-p*
	   #:*compiler-auto-stack-checks-p*
	   #:*compiler-local-segment-prefix*
	   #:*compiler-global-segment-prefix*
	   #:*compiler-physical-segment-prefix*
	   #:*compiler-nonlocal-lispval-read-segment-prefix*
	   #:*compiler-nonlocal-lispval-write-segment-prefix*
	   #:*compiler-compile-eval-whens*
	   #:*compiler-compile-macro-expanders*
	   #:*compiler-allow-untagged-word-bits*
	   )
  (:import-from muerte
		muerte::translate-program
		muerte::decode-macro-lambda-list
		muerte::un-backquote
		muerte::unquote
		muerte::unquote-splicing
		muerte::unquote-nsplicing

		muerte::decode-optional-formal
		muerte::decode-keyword-formal
		muerte::parse-body
                muerte::parse-macro-lambda-list
		muerte::unfold-circular-list
		muerte::compute-function-block-name
		muerte::movitz-macroexpand
		muerte::movitz-macroexpand-1
		muerte::decode-optional-formal
		muerte::decode-keyword-formal
                ))
