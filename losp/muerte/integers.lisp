;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      integers.lisp
;;;; Description:   Arithmetics.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Wed Nov  8 18:44:57 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: integers.lisp,v 1.82 2004/07/21 22:30:51 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(require :muerte/basic-macros)
(require :muerte/typep)
(require :muerte/arithmetic-macros)
(provide :muerte/integers)

(in-package muerte)

(defconstant most-positive-fixnum #.movitz::+movitz-most-positive-fixnum+)
(defconstant most-negative-fixnum #.movitz::+movitz-most-negative-fixnum+)

;;; Comparison

(define-primitive-function fast-compare-two-reals (n1 n2)
  "Compare two numbers (i.e. set EFLAGS accordingly)."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :nothing) ; unspecified
	    (:testb ,movitz::+movitz-fixnum-zmask+ :al)
	    (:jnz 'n1-not-fixnum)
	    (:testb ,movitz::+movitz-fixnum-zmask+ :bl)
	    (:jnz 'n2-not-fixnum-but-n1-is)
	    (:cmpl :ebx :eax)		; both were fixnum
	    (:ret)
	   n1-not-fixnum		; but we don't know about n2
	    (:testb ,movitz::+movitz-fixnum-zmask+ :bl)
	    (:jnz 'neither-is-fixnum)
	    ;; n2 is fixnum
	    (:locally (:jmp (:edi (:edi-offset fast-compare-real-fixnum))))
	   n2-not-fixnum-but-n1-is
	    (:locally (:jmp (:edi (:edi-offset fast-compare-fixnum-real))))
	   neither-is-fixnum
	    ;; Check that both numbers are bignums, and compare them.
	    (:leal (:eax ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jnz '(:sub-program (n1-not-bignum)
		    (:int 64)))
	    (:movl (:eax ,movitz:+other-type-offset+) :ecx)
	    (:cmpb ,(movitz:tag :bignum) :cl)
	    (:jne 'n1-not-bignum)

	    (:cmpl :eax :ebx)		; If they are EQ, they are certainly =
	    (:je '(:sub-program (n1-and-n2-are-eq)
		   (:ret)))

	    (:leal (:ebx ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jnz '(:sub-program (n2-not-bignum)
		    (:movl :ebx :eax)
		    (:int 64)))
	    (:movl (:ebx ,movitz:+other-type-offset+) :ecx)
	    (:cmpb ,(movitz:tag :bignum) :cl)
	    (:jne 'n2-not-bignum)

	    (:cmpb :ch (:eax (:offset movitz-bignum sign)))
	    (:jne '(:sub-program (different-signs)
		    ;; Comparing the sign-bytes sets up EFLAGS correctly!
		    (:ret)))
	    (:testl #xff00 :ecx)
	    (:jnz 'compare-negatives)
	    ;; Both n1 and n2 are positive bignums.

	    (:shrl 16 :ecx)
	    (:cmpw :cx (:eax (:offset movitz-bignum length)))
	    (:jne '(:sub-program (positive-different-sizes)
		    (:ret)))

	    ;; Both n1 and n2 are positive bignums of the same size, namely ECX.
	    (:movl :ecx :edx)		; counter
	   positive-compare-loop
	    (:subl ,movitz:+movitz-fixnum-factor+ :edx)
	    (:jz 'positive-compare-lsb)
	    (:movl (:ebx :edx (:offset movitz-bignum bigit0)) :ecx)
	    (:cmpl :ecx (:eax :edx (:offset movitz-bignum bigit0)))
	    (:je 'positive-compare-loop)
	   positive-compare-lsb
	    ;; Now we have to make the compare act as unsigned, which is why
	    ;; we compare zero-extended 16-bit quantities.
	    (:movzxw (:ebx :edx (:offset movitz-bignum bigit0 2)) :ecx) ; First compare upper 16 bits.
	    (:locally (:movl :ecx (:edi (:edi-offset scratch0))))
	    (:movzxw (:eax :edx (:offset movitz-bignum bigit0 2)) :ecx)
	    (:locally (:cmpl (:edi (:edi-offset scratch0)) :ecx))
	    (:jne 'upper-16-decisive)
	    (:movzxw (:ebx :edx (:offset movitz-bignum bigit0))
		     :ecx)		; Then compare lower 16 bits.
	    (:locally (:movl :ecx (:edi (:edi-offset scratch0))))
	    (:movzxw (:eax :edx (:offset movitz-bignum bigit0))
		     :ecx)		; Then compare lower 16 bits.
	    (:locally (:cmpl (:edi (:edi-offset scratch0)) :ecx))
	   upper-16-decisive
	    (:ret)
	    
	   compare-negatives
	    ;; Moth n1 and n2 are negative bignums.

	    (:shrl 16 :ecx)
	    (:cmpw (:eax (:offset movitz-bignum length)) :cx)
	    (:jne '(:sub-program (negative-different-sizes)
		    (:ret)))

	    ;; Both n1 and n2 are negative bignums of the same size, namely ECX.
	    (:movl :ecx :edx)		; counter
	   negative-compare-loop
	    (:subl ,movitz:+movitz-fixnum-factor+ :edx)
	    (:jz 'negative-compare-lsb)
	    (:movl (:eax :edx (:offset movitz-bignum bigit0)) :ecx)
	    (:cmpl :ecx (:ebx :edx (:offset movitz-bignum bigit0)))
	    (:je 'negative-compare-loop)
	    (:ret)
	   negative-compare-lsb		; it's down to the LSB bigits.
	    ;; Now we have to make the compare act as unsigned, which is why
	    ;; we compare zero-extended 16-bit quantities.
	    (:movzxw (:ebx :edx (:offset movitz-bignum bigit0 2))
		     :ecx)		; First compare upper 16 bits.
	    (:locally (:movl :ecx (:edi (:edi-offset scratch0))))
	    (:movzxw (:eax :edx (:offset movitz-bignum bigit0)) :ecx)
	    (:locally (:cmpl :ecx (:edi (:edi-offset scratch0))))
	    (:jne 'negative-upper-16-decisive)
	    (:movzxw (:ebx :edx (:offset movitz-bignum bigit0))
		     :ecx)		; Then compare lower 16 bits.
	    (:locally (:movl :ecx (:edi (:edi-offset scratch0))))
	    (:movzxw (:eax :edx (:offset movitz-bignum bigit0))
		     :ecx)		; Then compare lower 16 bits.
	    (:locally (:cmpl :ecx (:edi (:edi-offset scratch0))))
	   negative-upper-16-decisive
	    (:ret))))
    (do-it)))

(define-primitive-function fast-eql (x y)
  "Compare EAX and EBX under EQL, result in ZF.
Preserve EAX and EBX."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :nothing) ; unspecified
	    (:cmpl :eax :ebx)		; EQ?
	    (:je 'done)
	    (:leal (:eax ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jne 'done)
	    (:leal (:ebx ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jne 'done)
	    (:movl (:eax ,movitz:+other-type-offset+) :ecx)
	    (:cmpb ,(movitz:tag :bignum) :cl)
	    (:jne 'done)
	    (:cmpl :ecx (:ebx ,movitz:+other-type-offset+))
	    (:jne 'done)
	    ;; Ok.. we have two bignums of identical sign and size.
	    (:shrl 16 :ecx)
	    (:movl :ecx :edx)		; counter
	   compare-loop
	    (:subl ,movitz:+movitz-fixnum-factor+ :edx)
	    (:jz 'done)
	    (:movl (:eax :edx (:offset movitz-bignum bigit0 -4)) :ecx)
	    (:cmpl :ecx (:ebx :edx (:offset movitz-bignum bigit0 -4)))
	    (:je 'compare-loop)
	   done
	    (:ret))))
    (do-it)))

(define-primitive-function fast-compare-fixnum-real (n1 n2)
  "Compare (known) fixnum <n1> with real <n2>."
  (macrolet
      ((do-it ()
	 `(with-inline-assembly (:returns :nothing) ; unspecified
	    (:testb ,movitz::+movitz-fixnum-zmask+ :bl)
	    (:jnz 'n2-not-fixnum)
	    (:cmpl :ebx :eax)
	    (:ret)
	   n2-not-fixnum
	    (:leal (:ebx ,(- (movitz:tag :other))) :ecx)
	    (:testb 7 :cl)
	    (:jnz '(:sub-program (not-integer)
		    (:movl :ebx :eax)
		    (:int 64)))
	    (:movl (:ebx ,movitz:+other-type-offset+) :ecx)
	    (:cmpw ,(movitz:tag :bignum 0) :cx)
	    (:jne 'not-plusbignum)
	    ;; compare eax with something bigger
	    (:cmpl #x10000000 :edi)
	    (:ret)
	   not-plusbignum
	    (:cmpw ,(movitz:tag :bignum #xff) :cx)
	    (:jne 'not-integer)
	    ;; compare ebx with something bigger
	    (:cmpl #x-10000000 :edi)
	    (:ret))))
    (do-it)))

(define-primitive-function fast-compare-real-fixnum (n1 n2)
  "Compare real <n1> with fixnum <n2>."
  (with-inline-assembly (:returns :nothing) ; unspecified
    (:testb #.movitz::+movitz-fixnum-zmask+ :al)
    (:jnz 'not-fixnum)
    (:cmpl :ebx :eax)
    (:ret)
   not-fixnum
    (:leal (:eax #.(cl:- (movitz:tag :other))) :ecx)
    (:testb 7 :cl)
    (:jnz '(:sub-program (not-integer)
	    (:int 64)))
    (:movl (:eax #.movitz:+other-type-offset+) :ecx)
    (:cmpw #.(movitz:tag :bignum 0) :cx)
    (:jne 'not-plusbignum)
    ;; compare ebx with something bigger
    (:cmpl #x-10000000 :edi)
    (:ret)
   not-plusbignum
    (:cmpw #.(movitz:tag :bignum #xff) :cx)
    (:jne 'not-integer)
    ;; compare ebx with something bigger
    (:cmpl #x10000000 :edi)
    (:ret)))

;;;


(defmacro define-number-relational (name 2op-name condition &key (defun-p t) 3op-name)
  `(progn
     ,(when condition
	`(define-compiler-macro ,2op-name (n1 n2 &environment env)
	   (cond
	    ((and (movitz:movitz-constantp n1 env)
		  (movitz:movitz-constantp n2 env))
	     (list ',2op-name (movitz:movitz-eval n1 env) (movitz:movitz-eval n2 env)))
	    ((movitz:movitz-constantp n1 env)
	     (let ((n1 (movitz::movitz-eval n1 env)))
	       (check-type n1 number)
	       (if (typep n1 '(signed-byte 30))
		   `(with-inline-assembly (:returns ,,condition :side-effects nil)
		      (:compile-two-forms (:eax :ebx) ,n1 ,n2)
		      (:call-global-pf fast-compare-fixnum-real))
		 `(with-inline-assembly (:returns ,,condition :side-effects nil)
		    (:compile-two-forms (:eax :ebx) ,n1 ,n2)
		    (:call-global-pf fast-compare-two-reals)))))
	    ((movitz:movitz-constantp n2 env)
	     (let ((n2 (movitz:movitz-eval n2 env)))
	       (check-type n2 number)
	       (if (typep n2 '(signed-byte 30))
		   `(with-inline-assembly (:returns ,,condition :side-effects nil)
		      (:compile-two-forms (:eax :ebx) ,n1 ,n2)
		      (:call-global-pf fast-compare-real-fixnum))
		 `(with-inline-assembly (:returns ,,condition :side-effects nil)
		    (:compile-two-forms (:eax :ebx) ,n1 ,n2)
		    (:call-global-pf fast-compare-two-reals)))))
	    (t `(with-inline-assembly (:returns ,,condition :side-effects nil)
		  (:compile-two-forms (:eax :ebx) ,n1 ,n2)
		  (:call-global-pf fast-compare-two-reals))))))

     (defun ,2op-name (n1 n2)
       (,2op-name n1 n2))

     (define-compiler-macro ,name (&whole form number &rest more-numbers)
       (case (length more-numbers)
	 (0 `(progn ,number t))
	 (1 `(,',2op-name ,number ,(first more-numbers)))
	 ,@(when 3op-name
	     `((2 `(,',3op-name ,number ,(first more-numbers) ,(second more-numbers)))))
	 (t #+ignore (when (= 2 (length more-numbers))
		       (warn "3op: ~S" form))
	  `(and (,',2op-name ,number ,(first more-numbers))
		  (,',name ,@more-numbers)))))

     ,(when defun-p
	`(defun ,name (number &rest more-numbers)
	   (declare (dynamic-extent more-numbers))
	   (cond
	    ((null more-numbers)
	     (check-type number fixnum)
	     t)
	    ((not (cdr more-numbers))
	     (,2op-name number (first more-numbers)))
	    (t (and (,2op-name number (first more-numbers))
		    (do ((p more-numbers (cdr p)))
			((not (cdr p)) t)
		      (unless (,2op-name (car p) (cadr p))
			(return nil))))))))))

(define-number-relational >= >=%2op :boolean-greater-equal)
(define-number-relational > >%2op :boolean-greater)
(define-number-relational < <%2op :boolean-less)
(define-number-relational <= <=%2op :boolean-less-equal :3op-name <=%3op)

;;; Unsigned

(defun below (x max)
  "Is x between 0 and max?"
  (compiler-macro-call below x max))


;;; Equality

(define-compiler-macro =%2op (n1 n2 &environment env)
  (cond
   ((movitz:movitz-constantp n1 env)
    (let ((n1 (movitz:movitz-eval n1 env)))
      (etypecase n1
	((eql 0)
	 `(do-result-mode-case ()
	    (:booleans
	     (with-inline-assembly (:returns :boolean-zf=1 :side-effects nil)
	       (:compile-form (:result-mode :eax) ,n2)
	       (:testl :eax :eax)))
	    (t (with-inline-assembly (:returns :boolean-cf=1 :side-effects nil)
		 (:compile-form (:result-mode :eax) ,n2)
		 (:cmpl 1 :eax)))))
	((signed-byte 30)
	 `(with-inline-assembly (:returns :boolean-zf=1 :side-effects nil)
	    (:compile-two-forms (:eax :ebx) ,n1 ,n2)
	    (:call-global-pf fast-compare-fixnum-real)))
	(integer
	 `(with-inline-assembly (:returns :boolean-zf=1 :side-effects nil)
	    (:compile-two-forms (:eax :ebx) ,n1 ,n2)
	    (:call-global-pf fast-compare-two-reals))))))
   ((movitz:movitz-constantp n2 env)
    `(=%2op ,n2 ,n1))
   (t `(with-inline-assembly (:returns :boolean-zf=1 :side-effects nil)
	 (:compile-two-forms (:eax :ebx) ,n1 ,n2)
	 (:call-global-pf fast-compare-two-reals)))))

(define-number-relational = =%2op nil :defun-p nil)

(defun = (first-number &rest numbers)
  (declare (dynamic-extent numbers))
  (dolist (n numbers t)
    (unless (= first-number n)
      (return nil))))

(define-number-relational /= /=%2op :boolean-zf=0 :defun-p nil)

(defun /= (&rest numbers)
  (declare (dynamic-extent numbers))
  (do ((p (cdr numbers) (cdr p)))
      ((null p) t)
    (do ((v numbers (cdr v)))
	((eq p v))
      (when (= (car p) (car v))
	(return-from /= nil)))))


;;;;

(deftype positive-fixnum ()
  '(integer 0 #.movitz:+movitz-most-positive-fixnum+))

(deftype positive-bignum ()
  `(integer #.(cl:1+ movitz:+movitz-most-positive-fixnum+) *))

(deftype negative-fixnum ()
  `(integer #.movitz:+movitz-most-negative-fixnum+ -1))

(defun fixnump (x)
  (typep x 'fixnum))

(defun evenp (x)
  (compiler-macro-call evenp x))

(defun oddp (x)
  (compiler-macro-call oddp x))

;;; Types

(define-typep integer (x &optional (min '*) (max '*))
  (and (typep x 'integer)
       (or (eq min '*) (<= min x))
       (or (eq max '*) (<= x max))))

(deftype signed-byte (&optional (size '*))
  (cond
   ((eq size '*)
    'integer)
   ((typep size '(integer 1 *))
    (list 'integer
	  (- (ash 1 (1- size)))
	  (1- (ash 1 (1- size)))))
   (t (error "Illegal size for signed-byte."))))

(deftype unsigned-byte (&optional (size '*))
  (cond
   ((eq size '*)
    '(integer 0))
   ((typep size '(integer 1 *))
    (list 'integer 0 (1- (ash 1 size))))
   (t (error "Illegal size for unsigned-byte."))))

(define-simple-typep (bit bitp) (x)
  (or (eq x 0) (eq x 1)))

;;; 

(defun %negatef (x p0 p1)
  "Negate x. If x is not eq to p0 or p1, negate x destructively."
  (etypecase x
    (fixnum (- x))
    (bignum
     (if (or (eq x p0) (eq x p1))
	 (- x)
       (with-inline-assembly (:returns :eax)
	 (:compile-form (:result-mode :eax) x)
	 (:xorl #xff00 (:eax #.movitz:+other-type-offset+)))))))

;;; Addition

(defun + (&rest terms)
  (declare (without-check-stack-limit))
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (macrolet
	  ((do-it ()
	     `(number-double-dispatch (x y)
		((fixnum fixnum)
		 (with-inline-assembly (:returns :eax)
		   (:compile-form (:result-mode :eax) x)
		   (:compile-form (:result-mode :ebx) y)
		   (:addl :ebx :eax)
		   (:jo '(:sub-program (fix-fix-overflow)
			  (:movl :eax :ecx)
			  (:jns 'fix-fix-negative)
			  (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
			  (:call-local-pf box-u32-ecx)
			  (:jmp 'fix-fix-ok)
			  fix-fix-negative
			  (:jz 'fix-double-negative)
			  (:negl :ecx)
			  (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
			  (:call-local-pf box-u32-ecx)
			  (:movl ,(dpb 1 (byte 16 16)
				   (movitz:tag :bignum #xff))
			   (:eax ,movitz:+other-type-offset+))
			  (:jmp 'fix-fix-ok)
			  fix-double-negative
			  (:compile-form (:result-mode :eax)
			   ,(* 2 movitz:+movitz-most-negative-fixnum+))
			  (:jmp 'fix-fix-ok)))
		  fix-fix-ok))
		((positive-bignum positive-fixnum)
		 (+ y x))
		((positive-fixnum positive-bignum)
		 (bignum-add-fixnum y x)
		 #+ignore
		 (with-inline-assembly (:returns :eax :labels (retry-not-size1
							       not-size1
							       copy-bignum-loop
							       add-bignum-loop
							       add-bignum-done
							       no-expansion
							       pfix-pbig-done))
		   (:compile-two-forms (:eax :ebx) y x)
		   (:testl :ebx :ebx)
		   (:jz 'pfix-pbig-done)
		   (:movzxw (:eax (:offset movitz-bignum length)) :ecx)
		   (:cmpl ,movitz:+movitz-fixnum-factor+ :ecx)
		   (:jne 'not-size1)
		   (:compile-form (:result-mode :ecx) x)
		   (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:addl (:eax (:offset movitz-bignum bigit0)) :ecx)
		   (:jc 'retry-not-size1)
		   (:call-local-pf box-u32-ecx)
		   (:jmp 'pfix-pbig-done)
		  retry-not-size1
		   (:compile-form (:result-mode :eax) y)
		   (:movzxw (:eax (:offset movitz-bignum length)) :ecx)
		  not-size1
		   (:declare-label-set retry-jumper (retry-not-size1))
		   (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		   (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
				      'retry-jumper)
				    (:edi (:edi-offset atomically-status))))
		   (:leal ((:ecx 1) ,(* 2 movitz:+movitz-fixnum-factor+))
			  :eax)		; Number of words
		   (:call-local-pf get-cons-pointer)
		   (:load-lexical (:lexical-binding y) :ebx) ; bignum
		   (:movzxw (:ebx (:offset movitz-bignum length)) :ecx)
		   (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			  :edx)
		   (:movl 0 (:eax :edx ,movitz:+other-type-offset+)) ; MSB
		  copy-bignum-loop
		   (:subl ,movitz:+movitz-fixnum-factor+ :edx)
		   (:movl (:ebx :edx ,movitz:+other-type-offset+) :ecx)
		   (:movl :ecx (:eax :edx ,movitz:+other-type-offset+))
		   (:jnz 'copy-bignum-loop)

		   (:load-lexical (:lexical-binding x) :ecx)
		   (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:xorl :ebx :ebx)
		   (:addl :ecx (:eax (:offset movitz-bignum bigit0)))
		   (:jnc 'add-bignum-done)
		  add-bignum-loop
		   (:addl 4 :ebx)
		   (:addl 1 (:eax :ebx (:offset movitz-bignum bigit0)))
		   (:jc 'add-bignum-loop)
		  add-bignum-done
		   (:movzxw (:eax (:offset movitz-bignum length)) :ecx)
		   (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+) :ecx)
		   (:cmpl 0 (:eax :ecx (:offset movitz-bignum bigit0 -4)))
		   (:je 'no-expansion)
		   (:addl #x40000 (:eax ,movitz:+other-type-offset+))
		   (:addl ,movitz:+movitz-fixnum-factor+ :ecx)
		  no-expansion
		   (:call-local-pf cons-commit)
		   (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				    (:edi (:edi-offset atomically-status))))
		   
		  pfix-pbig-done))
		((positive-bignum negative-fixnum)
		 (+ y x))
		((negative-fixnum positive-bignum)
		 (with-inline-assembly (:returns :eax :labels (retry-not-size1
							       retry-jumper
							       not-size1
							       copy-bignum-loop
							       add-bignum-loop
							       add-bignum-done
							       no-expansion
							       pfix-pbig-done))
		   (:compile-two-forms (:eax :ebx) y x)
		   (:movzxw (:eax (:offset movitz-bignum length)) :ecx)
		   (:cmpl 4 :ecx)
		   (:jne 'not-size1)
		   (:compile-form (:result-mode :ecx) x)
		   (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:addl (:eax (:offset movitz-bignum bigit0)) :ecx)
		   (:call-local-pf box-u32-ecx)
		   (:jmp 'pfix-pbig-done)
		  retry-not-size1
		   (:compile-form (:result-mode :eax) y)
		   (:movzxw (:eax (:offset movitz-bignum length)) :ecx)
		  not-size1
		   (:declare-label-set retry-jumper (retry-not-size1))
		   (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		   (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
				      'retry-jumper)
				    (:edi (:edi-offset atomically-status))))
		   (:leal ((:ecx 1) ,(* 1 movitz:+movitz-fixnum-factor+))
			  :eax)		; Number of words
		   (:call-local-pf get-cons-pointer)
		   (:load-lexical (:lexical-binding y) :ebx) ; bignum
		   (:movzxw (:ebx (:offset movitz-bignum length)) :ecx)
		   (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			  :edx)
		  copy-bignum-loop
		   (:subl ,movitz:+movitz-fixnum-factor+ :edx)
		   (:movl (:ebx :edx ,movitz:+other-type-offset+) :ecx)
		   (:movl :ecx (:eax :edx ,movitz:+other-type-offset+))
		   (:jnz 'copy-bignum-loop)

		   (:load-lexical (:lexical-binding x) :ecx)
		   (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:xorl :ebx :ebx)	; counter
		   (:negl :ecx)
		   (:subl :ecx (:eax (:offset movitz-bignum bigit0)))
		   (:jnc 'add-bignum-done)
		  add-bignum-loop
		   (:addl 4 :ebx)
		   (:subl 1 (:eax :ebx (:offset movitz-bignum bigit0)))
		   (:jc 'add-bignum-loop)
		  add-bignum-done
		   (:movzxw (:eax (:offset movitz-bignum length))
			    :ecx)
		   (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			  :ecx)		; result bignum word-size
		   (:cmpl 0 (:eax :ecx (:offset movitz-bignum bigit0 -8)))
		   (:jne 'no-expansion)
		   (:subl #x40000 (:eax ,movitz:+other-type-offset+))
		   (:subl ,movitz:+movitz-fixnum-factor+ :ecx)
		  no-expansion
		   (:call-local-pf cons-commit)
		   (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				    (:edi (:edi-offset atomically-status))))
		   
		  pfix-pbig-done))
		((positive-bignum positive-bignum)
		 (if (< (%bignum-bigits y) (%bignum-bigits x))
		     (+ y x)
		   ;; Assume x is smallest.
		   (with-inline-assembly (:returns :eax :labels (retry-not-size1
								 retry-jumper
								 not-size1
								 copy-bignum-loop
								 add-bignum-loop
								 add-bignum-done
								 no-expansion
								 pfix-pbig-done
								 zero-padding-loop))
		     (:compile-two-forms (:eax :ebx) y x)
		     (:testl :ebx :ebx)
		     (:jz 'pfix-pbig-done)
		     (:movzxw (:eax (:offset movitz-bignum length)) :ecx)
		     (:cmpl ,movitz:+movitz-fixnum-factor+ :ecx)
		     (:jne 'not-size1)
		     (:movl (:ebx (:offset movitz-bignum bigit0)) :ecx)
		     (:addl (:eax (:offset movitz-bignum bigit0)) :ecx)
		     (:jc 'retry-not-size1)
		     (:call-local-pf box-u32-ecx)
		     (:jmp 'pfix-pbig-done)
		    retry-not-size1
		     (:compile-form (:result-mode :eax) y)
		     (:movzxw (:eax (:offset movitz-bignum length)) :ecx)
		    not-size1
		     (:declare-label-set retry-jumper (retry-not-size1))
		     (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		     (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
					'retry-jumper)
				      (:edi (:edi-offset atomically-status))))
		     (:leal ((:ecx 1) ,(* 2 movitz:+movitz-fixnum-factor+))
			    :eax)	; Number of words
		     (:call-local-pf get-cons-pointer)
		     (:load-lexical (:lexical-binding y) :ebx) ; bignum
		     (:movzxw (:ebx (:offset movitz-bignum length)) :ecx)
		     (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			    :edx)
		     (:movl 0 (:eax :edx ,movitz:+other-type-offset+)) ; MSB
		    copy-bignum-loop
		     (:subl ,movitz:+movitz-fixnum-factor+ :edx)
		     (:movl (:ebx :edx ,movitz:+other-type-offset+) :ecx)
		     (:movl :ecx (:eax :edx ,movitz:+other-type-offset+))
		     (:jnz 'copy-bignum-loop)

		     (:load-lexical (:lexical-binding x) :ebx)
		     (:xorl :edx :edx)	; counter
		     (:xorl :ecx :ecx)	; Carry
		    add-bignum-loop
		     (:cmpw :dx (:ebx (:offset movitz-bignum length)))
		     (:jbe '(:sub-program (zero-padding-loop)
			     (:addl :ecx (:eax :edx (:offset movitz-bignum
						      bigit0)))
			     (:sbbl :ecx :ecx)
			     (:negl :ecx) ; ECX = Add's Carry.
			     (:addl 4 :edx)
			     (:cmpw :dx (:eax (:offset movitz-bignum length)))
			     (:jae 'zero-padding-loop)
			     (:jmp 'add-bignum-done)))
		     (:addl (:ebx :edx (:offset movitz-bignum bigit0))
			    :ecx)
		     (:jc '(:sub-program (term1-carry)
			    ;; The digit + carry carried over, ECX = 0
			    (:addl 1 :ecx)
			    (:addl 4 :edx)
			    (:cmpw :dx (:eax (:offset movitz-bignum length)))
			    (:jae 'add-bignum-loop)
			    (:jmp 'add-bignum-done)))
		     (:addl :ecx (:eax :edx (:offset movitz-bignum bigit0)))
		     (:sbbl :ecx :ecx)
		     (:negl :ecx)	; ECX = Add's Carry.
		     (:addl 4 :edx)
		     (:cmpw :dx (:eax (:offset movitz-bignum length)))
		     (:jae 'add-bignum-loop)
		    add-bignum-done
		     (:movzxw (:eax (:offset movitz-bignum length))
			      :ecx)
		     (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			    :ecx)
		     (:cmpl 0 (:eax :ecx (:offset movitz-bignum bigit0 -4)))
		     (:je 'no-expansion)
		     (:addl #x40000 (:eax ,movitz:+other-type-offset+))
		     (:addl ,movitz:+movitz-fixnum-factor+ :ecx)
		    no-expansion
		     (:call-local-pf cons-commit)
		     (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				      (:edi (:edi-offset atomically-status))))
		    pfix-pbig-done)
		   ))
		(((integer * -1) (integer 0 *))
		 (- y (- x)))
		(((integer 0 *) (integer * -1))
		 (- x (- y)))
		(((integer * -1) (integer * -1))
		 (%negatef (+ (- x) (- y)) x y))
		)))
	(do-it)))
   (t (&rest terms)
      (declare (dynamic-extent terms))
      (if (null terms)
	  0
	(reduce #'+ terms)))))

(defun 1+ (number)
  (+ 1 number))

(defun 1- (number)
  (+ -1 number))

;;; Subtraction

(defun - (minuend &rest subtrahends)
  (declare (dynamic-extent subtrahends))
  (numargs-case
   (1 (x)
      (macrolet
	  ((do-it ()
	     `(with-inline-assembly (:returns :eax)
		(:compile-form (:result-mode :eax) x)
		(:testb ,movitz:+movitz-fixnum-zmask+ :al)
		(:jnz '(:sub-program (not-fixnum)
			(:leal (:eax ,(- (movitz:tag :other))) :ecx)
			(:testb 7 :cl)
			(:jnz '(:sub-program (not-a-number)
				(:compile-form (:result-mode :ignore)
				 (error 'type-error :expected-type 'number :datum x))))
			(:movl (:eax ,movitz:+other-type-offset+) :ecx)
			(:cmpb ,(movitz:tag :bignum) :cl)
			(:jne 'not-a-number)
			(:cmpl ,(dpb 4 (byte 16 16) (movitz:tag :bignum 0)) :ecx)
			(:jne 'not-most-negative-fixnum)
			(:cmpl ,(- most-negative-fixnum) (:eax (:offset movitz-bignum bigit0)))
			(:jne 'not-most-negative-fixnum)
			(:movl ,(ldb (byte 32 0)
				 (* most-negative-fixnum movitz::+movitz-fixnum-factor+))
			 :eax)
			(:jmp 'fix-ok)
			not-most-negative-fixnum
			(:compile-form (:result-mode :eax)
			 (copy-bignum x))
			(:notb (:eax (:offset movitz-bignum sign)))
			(:jmp 'fix-ok)))
		(:negl :eax)
		(:jo '(:sub-program (fix-overflow)
		       (:compile-form (:result-mode :eax)
			,(1+ movitz:+movitz-most-positive-fixnum+))
		       (:jmp 'fix-ok)))
	       fix-ok
		)))
	(do-it)))
   (2 (minuend subtrahend)
      (macrolet
	  ((do-it ()
	     `(number-double-dispatch (minuend subtrahend)
		((t (eql 0))
		 minuend)
		(((eql 0) t)
		 (- subtrahend))
		((fixnum fixnum)
		 (with-inline-assembly (:returns :eax :labels (done negative-result))
		   (:compile-two-forms (:eax :ebx) minuend subtrahend)
		   (:subl :ebx :eax)
		   (:jno 'done)
		   (:jnc 'negative-result)
		   (:movl :eax :ecx)
		   (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:orl ,(- movitz:+movitz-most-negative-fixnum+) :ecx)
		   (:call-local-pf box-u32-ecx)
		   (:jmp 'done)
		  negative-result
		   (:movl :eax :ecx)
		   (:negl :ecx)
		   (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:call-local-pf box-u32-ecx)
		   (:xorl #xff00 (:eax (:offset movitz-bignum type)))
		  done))
		((positive-bignum fixnum)
		 (+ (- subtrahend) minuend))
		((fixnum positive-bignum)
		 (%negatef (+ subtrahend (- minuend))
			   subtrahend minuend))
;;;		((positive-fixnum positive-bignum)
;;;		 (bignum-canonicalize
;;;		  (%bignum-negate
;;;		   (bignum-subf (copy-bignum subtrahend) minuend))))
;;;		((negative-fixnum positive-bignum)
;;;		 (bignum-canonicalize
;;;		  (%negatef (bignum-add-fixnum subtrahend minuend)
;;;			    subtrahend minuend)))
		((positive-bignum positive-bignum)
		 (cond
		  ((= minuend subtrahend)
		   0)
		  ((< minuend subtrahend)
		   (let ((x (- subtrahend minuend)))
		     (%negatef x subtrahend minuend)))
		  (t (bignum-canonicalize
		      (with-inline-assembly (:returns :eax)
			(:compile-two-forms (:eax :ebx) (copy-bignum minuend) subtrahend)
			(:xorl :edx :edx) ; counter
			(:xorl :ecx :ecx) ; carry
		       sub-loop
			(:addl (:ebx :edx (:offset movitz-bignum bigit0))
			       :ecx)
			(:jc '(:sub-program (carry-overflow)
			       ;; Just propagate carry
			       (:addl 1 :ecx)
			       (:addl 4 :edx)
			       (:cmpw :dx (:ebx (:offset movitz-bignum length)))
			       (:jne 'sub-loop)
			       (:jmp 'bignum-sub-done)))
			(:subl :ecx (:eax :edx (:offset movitz-bignum bigit0)))
			(:sbbl :ecx :ecx)
			(:negl :ecx)
			(:addl 4 :edx)
			(:cmpw :dx (:ebx (:offset movitz-bignum length)))
			(:jne 'sub-loop)
			(:subl :ecx (:eax :edx (:offset movitz-bignum bigit0)))
			(:jnc 'bignum-sub-done)
		       propagate-carry
			(:addl 4 :edx)
			(:subl 1 (:eax :edx (:offset movitz-bignum bigit0)))
			(:jc 'propagate-carry)
		       bignum-sub-done
			)))))
		(((integer 0 *) (integer * -1))
		 (+ minuend (- subtrahend)))
		(((integer * -1) (integer 0 *))
		 (%negatef (+ (- minuend) subtrahend) minuend subtrahend))
		(((integer * -1) (integer * -1))
		 (+ minuend (- subtrahend)))
		)))
	(do-it)))
   (t (minuend &rest subtrahends)
      (declare (dynamic-extent subtrahends))
      (if subtrahends
	  (reduce #'- subtrahends :initial-value minuend)
	(- minuend)))))

;;;

(defun zerop (number)
  (= 0 number))

(defun plusp (number)
  (> number 0))

(defun minusp (number)
  (< number 0))

(defun abs (x)
  (compiler-macro-call abs x))

(defun signum (x)
  (cond
   ((> x 0) 1)
   ((< x 0) -1)
   (t 0)))

;;;

(defun max (number1 &rest numbers)
  (numargs-case
   (2 (x y)
      (compiler-macro-call max x y))
   (t (number1 &rest numbers)
      (declare (dynamic-extent numbers))
      (let ((max number1))
	(dolist (x numbers max)
	  (when (> x max)
	    (setq max x)))))))

(defun min (number1 &rest numbers)
  (numargs-case
   (2 (x y)
      (compiler-macro-call min x y))
   (t (number1 &rest numbers)
      (declare (dynamic-extent numbers))
      (let ((min number1))
	(dolist (x numbers min)
	  (when (< x min)
	    (setq min x)))))))

;; shift 

(defun ash (integer count)
  (cond
   ((= 0 count)
    integer)
   ((= 0 integer) 0)
   ((plusp count)
    (let ((result-length (+ (integer-length integer) count)))
      (cond
       ((<= result-length 29)
	(with-inline-assembly (:returns :eax)
	  (:compile-two-forms (:eax :ecx) integer count)
	  (:shrl #.movitz:+movitz-fixnum-shift+ :ecx)
	  (:shll :cl :eax)))
       (t (check-type integer (integer 0 *))
	  (let ((result (%make-bignum (ceiling result-length 32))))
	    (dotimes (i (* 2 (%bignum-bigits result)))
	      (setf (memref result -2 i :unsigned-byte16)
		(let ((pos (- (* i 16) count)))
		  (cond
		   ((minusp (+ pos 16)) 0)
		   ((<= 0 pos)
		    (ldb (byte 16 pos) integer))
		   (t (ash (ldb (byte (+ pos 16) 0) integer)
			   (- pos)))))))
	    (assert (or (plusp (memref result -2 (+ -1 (* 2 (%bignum-bigits result))) :unsigned-byte16))
			(plusp (memref result -2 (+ -2 (* 2 (%bignum-bigits result))) :unsigned-byte16))))
	    (bignum-canonicalize result))))))
   (t (let ((count (- count)))
	(etypecase integer
	  (fixnum
	   (with-inline-assembly (:returns :eax :type fixnum)
	     (:compile-two-forms (:eax :ecx) integer count)
	     (:shrl #.movitz:+movitz-fixnum-shift+ :ecx)
	     (:std)
	     (:sarl :cl :eax)
	     (:andl -4 :eax)
	     (:cld)))
	  (positive-bignum
	   (let ((result-length (- (integer-length integer) count)))
	     (cond
	      ((<= result-length 1)
	       result-length)		; 1 or 0.
	      (t (multiple-value-bind (long short)
		     (truncate count 16)
		   (let ((result (%make-bignum (1+ (ceiling result-length 32)))))
		     (let ((src-max-bigit (* 2 (%bignum-bigits integer))))
		       (dotimes (i (* 2 (%bignum-bigits result)))
			 (let ((src (+ i long)))
			   (setf (memref result -2 i :unsigned-byte16)
			     (if (< src src-max-bigit)
				 (memref integer -2 src :unsigned-byte16)
			       0)))))
		     (bignum-canonicalize
		      (macrolet
			  ((do-it ()
			     `(with-inline-assembly (:returns :ebx)
				(:compile-two-forms (:ecx :ebx) short result)
				(:xorl :edx :edx) ; counter
				(:xorl :eax :eax) ; We need to use EAX for u32 storage.
				(:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
				(:std)
			       shift-short-loop
				(:addl 4 :edx)
				(:cmpw :dx (:ebx (:offset movitz-bignum length)))
				(:jbe 'end-shift-short-loop)
				(:movl (:ebx :edx (:offset movitz-bignum bigit0))
				       :eax)
				(:shrdl :cl :eax
					(:ebx :edx (:offset movitz-bignum bigit0 -4)))
				(:jmp 'shift-short-loop)
			       end-shift-short-loop
				(:movl :edx :eax) ; Safe EAX
				(:shrl :cl (:ebx :edx (:offset movitz-bignum bigit0 -4)))
				(:cld))))
			(do-it))))))))))))))

;;;;

(defun integer-length (integer)
  "=> number-of-bits"
  (etypecase integer
    (fixnum
     (macrolet
	 ((do-it ()
	    `(with-inline-assembly (:returns :eax)
	       (:xorl :eax :eax)
	       (:compile-form (:result-mode :ecx) integer)
	       (:testl :ecx :ecx)
	       (:jns 'not-negative)
	       (:notl :ecx)
	      not-negative
	       (:bsrl :ecx :ecx)
	       (:jz 'zero)
	       (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)
		       ,(* -1 movitz:+movitz-fixnum-factor+))
		      :eax)
	      zero)))
       (do-it)))
    (positive-bignum
     (macrolet
	 ((do-it ()
	    `(with-inline-assembly (:returns :eax)
	       (:compile-form (:result-mode :ebx) integer)
	       (:movzxw (:ebx (:offset movitz-bignum length))
			:edx)
	       (:xorl :eax :eax)
	      bigit-scan-loop
	       (:subl 4 :edx)
	       (:jc 'done)
	       (:cmpl 0 (:ebx :edx (:offset movitz-bignum bigit0)))
	       (:jz 'bigit-scan-loop)
	       ;; Now, EAX must be loaded with (+ (* EDX 32) bit-index 1).
	       (:leal ((:edx 8)) :eax)	; Factor 8
	       (:bsrl (:ebx :edx (:offset movitz-bignum bigit0))
		      :ecx)
	       (:leal ((:eax 4)) :eax)	; Factor 4
	       (:leal ((:ecx 4) :eax 4) :eax)
	      done)))
       (do-it)))
    (negative-bignum
     (let ((abs-length (bignum-integer-length integer)))
       (if (= 1 (bignum-logcount integer))
	   (1- abs-length)
	 abs-length)))))

;;; Multiplication

(defun * (&rest factors)
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (macrolet
	  ((do-it ()
	     `(number-double-dispatch (x y)
		((fixnum fixnum)
		 (let (d0 d1)
		   (with-inline-assembly (:returns :eax)
		     (:compile-two-forms (:eax :ecx) x y)
		     (:sarl ,movitz::+movitz-fixnum-shift+ :ecx)
		     (:std)
		     (:imull :ecx :eax :edx)
		     (:jno 'fixnum-result) ; most likely/optimized path.
		     (:cmpl ,movitz::+movitz-fixnum-factor+ :edx)
		     (:jc 'u32-result)
		     (:cmpl #xfffffffc :edx)
		     (:ja 'u32-negative-result)
		     (:jne 'two-bigits)
		     (:testl :eax :eax)
		     (:jnz 'u32-negative-result)
		     ;; The result requires 2 bigits..
		    two-bigits
		     (:shll ,movitz::+movitz-fixnum-shift+ :edx) ; guaranteed won't overflow.
		     (:cld)
		     (:store-lexical (:lexical-binding d0) :eax :type fixnum)
		     (:store-lexical (:lexical-binding d1) :edx :type fixnum)
		     (:compile-form (:result-mode :eax)
				    (malloc-non-pointer-words 3))
		     (:movl ,(dpb (* 2 movitz:+movitz-fixnum-factor+)
				  (byte 16 16) (movitz:tag :bignum 0))
			    (:eax ,movitz:+other-type-offset+))
		     (:load-lexical (:lexical-binding d0) :ecx)
		     (:movl :ecx (:eax (:offset movitz-bignum bigit0)))
		     (:load-lexical (:lexical-binding d1) :ecx)
		     (:sarl ,movitz:+movitz-fixnum-shift+
			    :ecx)
		     (:shrdl ,movitz:+movitz-fixnum-shift+ :ecx
			     (:eax (:offset movitz-bignum bigit0)))
		     (:sarl ,movitz:+movitz-fixnum-shift+
			    :ecx)
		     (:movl :ecx (:eax (:offset movitz-bignum bigit0 4)))
		     (:jns 'fixnum-done)
		     ;; if result was negative, we must negate bignum
		     (:notl (:eax (:offset movitz-bignum bigit0 4)))
		     (:negl (:eax (:offset movitz-bignum bigit0)))
		     (:cmc)
		     (:adcl 0 (:eax (:offset movitz-bignum bigit0 4)))
		     (:xorl #xff00 (:eax ,movitz:+other-type-offset+))
		     (:jmp 'fixnum-done)
		     
		    u32-result
		     (:movl :eax :ecx)
		     (:shrdl ,movitz::+movitz-fixnum-shift+ :edx :ecx)
		     (:movl :edi :edx)
		     (:cld)
		     (:call-local-pf box-u32-ecx)
		     (:jmp 'fixnum-done)
		     
		    u32-negative-result
		     (:movl :eax :ecx)
		     (:shrdl ,movitz::+movitz-fixnum-shift+ :edx :ecx)
		     (:movl :edi :edx)
		     (:cld)
		     (:negl :ecx)
		     (:call-local-pf box-u32-ecx)
		     (:xorl #xff00 (:eax ,movitz:+other-type-offset+))
		     (:jmp 'fixnum-done)

		    fixnum-result
		     (:movl :edi :edx)
		     (:cld)
		    fixnum-done)))
		(((eql 0) t) 0)
		(((eql 1) t) y)
		(((eql -1) t) (- y))
		((t fixnum) (* y x))
		((fixnum bignum)
		 (let (r)
		   (with-inline-assembly (:returns :eax)
		    retry
		     (:declare-label-set retry-jumper (retry))
		     (:compile-two-forms (:eax :ebx) (integer-length x) (integer-length y))
		     (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		     (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
					'retry-jumper)
				      (:edi (:edi-offset atomically-status))))
		     ;; Compute (1+ (ceiling (+ (len x) (len y)) 32)) ..
		     (:leal (:eax :ebx ,(* 4 (+ 31 32))) :eax)
		     (:andl ,(logxor #xffffffff (* 31 4)) :eax)
		     (:shrl 5 :eax)
		     (:call-local-pf get-cons-pointer) ; New bignum into EAX

		     (:load-lexical (:lexical-binding y) :ebx) ; bignum
		     (:movl (:ebx ,movitz:+other-type-offset+) :ecx)
		     (:movl :ecx (:eax ,movitz:+other-type-offset+))
		     (:store-lexical (:lexical-binding r) :eax :type bignum)

		     (:movl :eax :ebx)	; r into ebx
		     (:xorl :ecx :ecx)	; counter
		     (:xorl :edx :edx)	; initial carry
		     (:std)		; Make EAX, EDX, ESI non-GC-roots.
		     (:compile-form (:result-mode :esi) x)
		     (:sarl ,movitz:+movitz-fixnum-shift+ :esi)
		     (:jns 'multiply-loop)
		     (:negl :esi)	; can't overflow
		    multiply-loop
		     (:movl :edx (:ebx (:ecx 1) ; new
				       (:offset movitz-bignum bigit0)))
		     (:compile-form (:result-mode :ebx) y)
		     (:movl (:ebx (:ecx 1) (:offset movitz-bignum bigit0))
			    :eax)
		     
		     (:mull :esi :eax :edx)
		     (:compile-form (:result-mode :ebx) r)
		     (:addl :eax (:ebx :ecx (:offset movitz-bignum bigit0)))
		     (:adcl 0 :edx)
		     (:addl 4 :ecx)
		     (:cmpw :cx (:ebx (:offset movitz-bignum length)))
		     (:ja 'multiply-loop)
		     (:testl :edx :edx)
		     (:jz 'no-carry-expansion)
		     (:movl :edx (:ebx :ecx (:offset movitz-bignum bigit0)))
		     (:addl 4 :ecx)
		     (:movw :cx (:ebx (:offset movitz-bignum length)))
		    no-carry-expansion
		     (:movl (:ebp -4) :esi)
		     (:movl :ebx :eax)
		     (:movl :edi :edx)
		     (:cld)		; EAX, EDX, and ESI are GC roots again.
		     (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			    :ecx)
		     (:call-local-pf cons-commit)
		     (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				      (:edi (:edi-offset atomically-status))))
		     (:compile-form (:result-mode :ebx) x)
		     (:testl :ebx :ebx)
		     (:jns 'positive-result)
		     ;; Negate the resulting bignum
		     (:xorl #xff00 (:eax ,movitz:+other-type-offset+))
		    positive-result
		     )))
		((positive-bignum positive-bignum)
		 (if (< x y)
		     (* y x)
		   ;; X is the biggest factor.
		   #-movitz-reference-code
		   (do ((tmp (%make-bignum (ceiling (+ (integer-length x)
						       (integer-length y))
						    32)))
			(r (bignum-set-zerof (%make-bignum (ceiling (+ (integer-length x)
								       (integer-length y))
								    32))))
			(length (integer-length y))
			(i 0 (+ i 29)))
		       ((>= i length) (bignum-canonicalize r))
		     (bignum-set-zerof tmp)
		     (bignum-addf r (bignum-shift-leftf (bignum-mulf (bignum-addf tmp x)
								     (ldb (byte 29 i) y))
							i)))
		   #+movitz-reference-code
		   (do ((r 0)
			(length (integer-length y))
			(i 0 (+ i 29)))
		       ((>= i length) r)
		     (incf r (ash (* x (ldb (byte 29 i) y)) i)))))
		((t (integer * -1))
		 (%negatef (* x (- y)) x y))
		(((integer * -1) t)
		 (%negatef (* (- x) y) x y))
		(((integer * -1) (integer * -1))
		 (* (- x) (- y))))))
	(do-it)))
   (t (&rest factors)
      (declare (dynamic-extent factors))
      (if (null factors)
	  1
	(reduce '* factors)))))

;;; Division

(defun truncate (number &optional (divisor 1))
  (numargs-case
   (1 (number)
      (values number 0))
   (t (number divisor)
      (number-double-dispatch (number divisor)
	((t (eql 1))
	 (values number 0))
	((fixnum fixnum)
	 (with-inline-assembly (:returns :multiple-values)
	   (:compile-form (:result-mode :eax) number)
	   (:compile-form (:result-mode :ebx) divisor)
	   (:std)
	   (:cdq :eax :edx)
	   (:idivl :ebx :eax :edx)
	   (:shll #.movitz::+movitz-fixnum-shift+ :eax)
	   (:cld)
	   (:movl :edx :ebx)
	   (:xorl :ecx :ecx)
	   (:movb 2 :cl)		; return values: qutient, remainder.
	   (:stc)))
	((positive-fixnum positive-bignum)
	 (values 0 number))
	((positive-bignum positive-fixnum)
	 (macrolet
	     ((do-it ()
		`(let (r n)
		   (with-inline-assembly (:returns :multiple-values)
		     (:compile-form (:result-mode :ebx) number)
		     (:cmpw ,movitz:+movitz-fixnum-factor+
			    (:ebx (:offset movitz-bignum length)))
		     (:jne 'not-size1)
		     (:compile-form (:result-mode :ecx) divisor)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:std)
		     (:movl (:ebx (:offset movitz-bignum bigit0)) :eax)
		     (:xorl :edx :edx)
		     (:divl :ecx :eax :edx)
		     (:movl :eax :ecx)
		     (:shll ,movitz:+movitz-fixnum-shift+ :edx)
		     (:movl :edi :eax)
		     (:cld)
		     (:pushl :edx)
		     (:call-local-pf box-u32-ecx)
		     (:popl :ebx)
		     (:jmp 'done)
		    not-size1
		     (:xorl :eax :eax)
		     (:compile-form (:result-mode :ebx) number)
		     (:movw (:ebx (:offset movitz-bignum length)) :ax)
		     (:declare-label-set retry-jumper (not-size1))
		     (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		     (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
					'retry-jumper)
				      (:edi (:edi-offset atomically-status))))
		     (:addl 4 :eax)
		     (:call-local-pf get-cons-pointer) ; New bignum into EAX

		     (:store-lexical (:lexical-binding r) :eax :type bignum) ; XXX breaks GC invariant!
		     (:compile-form (:result-mode :ebx) number)
		     (:movl (:ebx ,movitz:+other-type-offset+) :ecx)
		     (:movl :ecx (:eax ,movitz:+other-type-offset+))
		     (:shrl 16 :ecx)
	     
		     (:xorl :edx :edx)	; edx=hi-digit=0
					; eax=lo-digit=msd(number)
		     (:std)
		     (:compile-form (:result-mode :esi) divisor)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :esi)

		    divide-loop
		     (:load-lexical (:lexical-binding number) :ebx)
		     (:movl (:ebx :ecx (:offset movitz-bignum bigit0 -4))
			    :eax)
		     (:divl :esi :eax :edx)
		     (:load-lexical (:lexical-binding r) :ebx)
		     (:movl :eax (:ebx :ecx (:offset movitz-bignum bigit0 -4)))
		     (:subl 4 :ecx)
		     (:jnz 'divide-loop)
		     (:movl :edi :eax)	; safe value
		     (:leal ((:edx ,movitz:+movitz-fixnum-factor+)) :edx)
		     (:movl (:ebp -4) :esi)
		     (:cld)
		     (:movl :ebx :eax)
		     (:movl :edx :ebx)

		     (:movzxw (:eax (:offset movitz-bignum length))
			      :ecx)
		     (:leal ((:ecx 1) ,movitz:+movitz-fixnum-factor+)
			    :ecx)
		     (:cmpl 0 (:eax :ecx (:offset movitz-bignum bigit0 -8)))
		     (:jne 'no-more-shrinkage)
		     
		     (:subw 4 (:eax (:offset movitz-bignum length)))
		     (:subl ,movitz:+movitz-fixnum-factor+ :ecx)
		     (:cmpl ,(* 2 movitz:+movitz-fixnum-factor+) :ecx)
		     (:jne 'no-more-shrinkage)
		     (:cmpl ,movitz:+movitz-most-positive-fixnum+
			    (:eax (:offset movitz-bignum bigit0)))
		     (:jnc 'no-more-shrinkage)
		     (:movl (:eax (:offset movitz-bignum bigit0))
			    :ecx)
		     (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
		     (:jmp 'fixnum-result) ; don't commit the bignum
		    no-more-shrinkage
		     (:call-local-pf cons-commit)
		    fixnum-result
		     (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				      (:edi (:edi-offset atomically-status))))	     
		    done
		     (:movl 2 :ecx)
		     (:stc)))))
	   (do-it)))
	((positive-bignum positive-bignum)
	 (cond
	  ((= number divisor) (values 1 0))
	  ((< number divisor) (values 0 number))
	  (t 
	   #-movitz-reference-code
	   (let* ((divisor-length (integer-length divisor))
		  (guess-pos (- divisor-length 29))
		  (msb (ldb (byte 29 guess-pos) divisor))) 
	     (when (eq msb most-positive-fixnum)
	       (incf guess-pos)
	       (setf msb (ash msb -1)))
	     (incf msb)
	     (do ((tmp (copy-bignum number))
		  (tmp2 (copy-bignum number))
		  (q (bignum-set-zerof (%make-bignum (ceiling (1+ (- (integer-length number)
								     divisor-length))
							      32))))
		  (r (copy-bignum number)))
		 ((%bignum< r divisor)
		  (values (bignum-canonicalize q)
			  (bignum-canonicalize r)))
	       (let ((guess (bignum-shift-rightf
			     (bignum-truncatef (bignum-addf (bignum-set-zerof tmp)
							    r)
					       msb)
			     guess-pos)))
		 (if (%bignum-zerop guess)
		     (setf q (bignum-addf-fixnum q 1)
			   r (bignum-subf r divisor))
		   (setf q (bignum-addf q guess)
			 r (do ((i 0 (+ i 29)))
			       ((>= i divisor-length) r)
			     (bignum-subf r (bignum-shift-leftf
					     (bignum-mulf (bignum-addf (bignum-set-zerof tmp2) guess)
							  (ldb (byte 29 i) divisor))
					     i))))))))
	   #+movitz-reference-code
	   (let* ((guess-pos (- (integer-length divisor) 29))
		  (msb (ldb (byte 29 guess-pos) divisor))) 
	     (when (eq msb most-positive-fixnum)
	       (incf guess-pos)
	       (setf msb (ash msb -1)))
	     (incf msb)
	     (do ((shift (- guess-pos))
		  (q 0)
		  (r number))
		 ((< r divisor)
		  (values q r))
	       (let ((guess (ash (truncate r msb) shift)))
		 (if (= 0 guess)
		     (setf q (1+ q)
			   r (- r divisor))
		   (setf q (+ q guess)
			 r (- r (* guess divisor))))))))))
	(((integer * -1) (integer 0 *))
	 (multiple-value-bind (q r)
	     (truncate (- number) divisor)
	   (values (%negatef q number divisor)
		   (%negatef r number divisor))))
	(((integer 0 *) (integer * -1))
	 (multiple-value-bind (q r)
	     (truncate number (- divisor))
	   (values (%negatef q number divisor)
		   r)))
	(((integer * -1) (integer * -1))
	 (multiple-value-bind (q r)
	     (truncate (- number) (- divisor))
	   (values q (%negatef r number divisor))))
	))))

(defun / (number &rest denominators)
  (numargs-case
   (1 (x)
      (make-rational 1 x))
   (2 (x y)
      (multiple-value-bind (q r)
	  (truncate x y)
	(if (= 0 r)
	    q
	  (make-rational x y))))
   (t (number &rest denominators)
      (declare (dynamic-extent denominators))
      (cond
       ((null denominators)
	(make-rational 1 number))
       ((null (cdr denominators))
	(multiple-value-bind (q r)
	    (truncate number (first denominators))
	  (if (= 0 r)
	      q
	    (make-rational number (first denominators)))))
       (t (/ number (reduce '* denominators)))))))
	       
(defun round (number &optional (divisor 1))
  "Mathematical rounding."
  (multiple-value-bind (quotient remainder)
      (truncate number divisor)
    (let ((rem2 (* 2 remainder)))
      (case (+ (if (minusp number) #b10 0)
	       (if (minusp divisor) #b01 0))
	(#b00 (cond
	       ((= divisor rem2)
		(if (evenp quotient)
		    (values quotient remainder)
		  (values (1+ quotient) (- remainder divisor))))
	       ((< rem2 divisor)
		(values quotient remainder))
	       (t (values (1+ quotient) (- remainder divisor)))))
	(#b11 (cond
	       ((= divisor rem2)
		(if (evenp quotient)
		    (values quotient remainder)
		  (values (1+ quotient) (- remainder divisor))))
	       ((> rem2 divisor)
		(values quotient remainder))
	       (t (values (1+ quotient) (- remainder divisor)))))
	(#b10 (cond
	       ((= (- divisor) rem2)
		(if (evenp quotient)
		    (values quotient remainder)
		  (values (1- quotient) (- remainder))))
	       ((< rem2 divisor)
		(values quotient remainder))
	       (t (values (1+ quotient) (- remainder divisor)))))
	(#b01 (cond
	       ((= (- divisor) rem2)
		(if (evenp quotient)
		    (values quotient remainder)
		  (values (1- quotient) (- remainder))))
	       ((> rem2 divisor)
		(values quotient remainder))
	       (t (values (1- quotient) (- remainder)))))))))

(defun ceiling (number &optional (divisor 1))
  (case (+ (if (minusp number) #b10 0)
	   (if (minusp divisor) #b01 0))
    (#b00 (multiple-value-bind (q r)
	      (truncate (+ number divisor -1) divisor)
	    (values q (- r (1- divisor)))))
    (t (error "Don't know."))))

(defun rem (dividend divisor)
  (nth-value 1 (truncate dividend divisor)))

(defun mod (number divisor)
  "Returns second result of FLOOR."
  (let ((rem (rem number divisor)))
    (if (and (not (zerop rem))
             (if (minusp divisor)
                 (plusp number)
                 (minusp number)))
        (+ rem divisor)
        rem)))

;;; bytes

(defun byte (size position)
  (check-type position (integer 0 #x3ff))
  (+ (* size #x400) position))

(defun byte-size (bytespec)
  (values (truncate bytespec #x400)))

(defun byte-position (bytespec)
  (rem bytespec #x400))

(defun logbitp (index integer)
  (check-type index positive-fixnum)
  (macrolet
      ((do-it ()
	 `(etypecase integer
	    (fixnum
	     (with-inline-assembly (:returns :boolean-cf=1)
	       (:compile-two-forms (:ecx :ebx) index integer)
	       (:shrl ,movitz::+movitz-fixnum-shift+ :ecx)
	       (:addl ,movitz::+movitz-fixnum-shift+ :ecx)
	       (:btl :ecx :ebx)))
	    (positive-bignum
	     (with-inline-assembly (:returns :boolean-cf=1)
	       (:compile-two-forms (:ecx :ebx) index integer)
	       (:shrl ,movitz::+movitz-fixnum-shift+ :ecx)
	       (:btl :ecx (:ebx (:offset movitz-bignum bigit0))))))))
    (do-it)))

(defun logand (&rest integers)
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (macrolet
	  ((do-it ()
	     `(number-double-dispatch (x y)
		((fixnum fixnum)
		 (with-inline-assembly (:returns :eax)
		   (:compile-two-forms (:eax :ebx) x y)
		   (:andl :ebx :eax)))
		((positive-bignum positive-fixnum)
		 (with-inline-assembly (:returns :eax)
		   (:compile-form (:result-mode :eax) x)
		   (:call-global-pf unbox-u32)
		   (:compile-form (:result-mode :eax) y)
		   (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :ecx)
		   (:andl :ecx :eax)))
		((positive-fixnum positive-bignum)
		 (with-inline-assembly (:returns :eax)
		   (:compile-form (:result-mode :eax) y)
		   (:call-global-pf unbox-u32)
		   (:compile-form (:result-mode :eax) x)
		   (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :ecx)
		   (:andl :ecx :eax)))
		((positive-bignum positive-bignum)
		 (if (< (%bignum-bigits y) (%bignum-bigits x))
		     (logand y x)
		   (bignum-canonicalize
		    (with-inline-assembly (:returns :eax)
		      (:compile-two-forms (:eax :ebx) (copy-bignum x) y)
		      (:movzxw (:eax (:offset movitz-bignum length))
			       :ecx)
		      (:leal ((:ecx 1) -4) :edx)
		     pb-pb-and-loop
		      (:movl (:ebx :edx (:offset movitz-bignum bigit0))
			     :ecx)
		      (:andl :ecx
			     (:eax :edx (:offset movitz-bignum bigit0)))
		      (:subl 4 :edx)
		      (:jnc 'pb-pb-and-loop)))))
		)))
	(do-it)))
   (t (&rest integers)
      (declare (dynamic-extent integers))
      (if (null integers)
	  -1
	(reduce #'logand integers)))))

(defun logandc1 (integer1 integer2)
  (macrolet
      ((do-it ()
	 `(number-double-dispatch (integer1 integer2)
	    ((t positive-fixnum)
	     (with-inline-assembly (:returns :eax :type fixnum)
	       (:compile-form (:result-mode :eax) integer1)
	       (:call-global-pf unbox-u32)
	       (:shll ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:compile-form (:result-mode :eax) integer2)
	       (:notl :ecx)
	       (:andl :ecx :eax)))
	    (((eql 0) t) integer2)
	    (((eql -1) t) 0)
	    ((positive-fixnum positive-bignum)
	     (bignum-canonicalize
	      (with-inline-assembly (:returns :eax)
		(:compile-two-forms (:eax :ecx) (copy-bignum integer2) integer1)
		(:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		(:notl :ecx)
		(:andl :ecx (:eax (:offset movitz-bignum bigit0))))))
	    ((positive-bignum positive-bignum)
	     (bignum-canonicalize
	      (with-inline-assembly (:returns :eax)
		(:compile-two-forms (:eax :ebx) (copy-bignum integer2) integer1)
		(:movzxw (:eax (:offset movitz-bignum length))
			 :ecx)
		(:leal ((:ecx 1) -4) :edx)
	       pb-pb-andc1-loop
		(:movl (:ebx :edx (:offset movitz-bignum bigit0))
		       :ecx)
		(:notl :ecx)
		(:andl :ecx (:eax :edx (:offset movitz-bignum bigit0)))
		(:subl 4 :edx)
		(:jnc 'pb-pb-andc1-loop)))))))
    (do-it)))


(defun logandc2 (integer1 integer2)
  (logandc1 integer2 integer1))

(defun logior (&rest integers)
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (number-double-dispatch (x y)
	((fixnum fixnum)
	 (with-inline-assembly (:returns :eax)
	   (:compile-two-forms (:eax :ebx) x y)
	   (:orl :ebx :eax)))
	((positive-fixnum positive-bignum)
	 (macrolet
	     ((do-it ()
		`(let ((r (copy-bignum y)))
		   (with-inline-assembly (:returns :eax)
		     (:compile-two-forms (:eax :ecx) r x)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:orl :ecx (:eax (:offset movitz-bignum bigit0)))))))
	   (do-it)))
	((positive-bignum positive-fixnum)
	 (macrolet
	     ((do-it ()
		`(let ((r (copy-bignum x)))
		   (with-inline-assembly (:returns :eax)
		     (:compile-two-forms (:eax :ecx) r y)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:orl :ecx (:eax (:offset movitz-bignum bigit0)))))))
	   (do-it)))
	((positive-bignum positive-bignum)
	 (if (< (%bignum-bigits x) (%bignum-bigits y))
	     (logior y x)
	   (let ((r (copy-bignum x)))
	     (macrolet
		 ((do-it ()
		    `(with-inline-assembly (:returns :eax)
		       (:compile-two-forms (:eax :ebx) r y)
		       (:movzxw (:ebx (:offset movitz-bignum length))
				:ecx)
		       (:leal ((:ecx 1) ,(* -1 movitz:+movitz-fixnum-factor+))
			      :edx)	; EDX is loop counter
		      or-loop
		       (:movl (:ebx :edx (:offset movitz-bignum bigit0))
			      :ecx)
		       (:orl :ecx
			     (:eax :edx (:offset movitz-bignum bigit0)))
		       (:subl 4 :edx)
		       (:jnc 'or-loop))))
	       (do-it)))))))
   (t (&rest integers)
      (declare (dynamic-extent integers))
      (if (null integers)
	  0
	(reduce #'logior integers)))))

(defun logxor (&rest integers)
  (numargs-case
   (1 (x) x)
   (2 (x y)
      (number-double-dispatch (x y)
	((fixnum fixnum)
	 (with-inline-assembly (:returns :eax)
	   (:compile-two-forms (:eax :ebx) x y)
	   (:xorl :ebx :eax)))
	(((eql 0) t) y)
	((t (eql 0)) x)
	((positive-fixnum positive-bignum)
	 (macrolet
	     ((do-it ()
		`(with-inline-assembly (:returns :eax)
		   (:compile-two-forms (:eax :ecx) (copy-bignum y) x)
		   (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:xorl :ecx (:eax (:offset movitz-bignum bigit0))))))
	   (do-it)))
	((positive-bignum positive-fixnum)
	 (macrolet
	     ((do-it ()
		`(with-inline-assembly (:returns :eax)
		   (:compile-two-forms (:eax :ecx) (copy-bignum x) y)
		   (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		   (:xorl :ecx (:eax (:offset movitz-bignum bigit0))))))
	   (do-it)))
	((positive-bignum positive-bignum)
	 (if (< (%bignum-bigits x) (%bignum-bigits y))
	     (logxor y x)
	   (let ((r (copy-bignum x)))
	     (macrolet
		 ((do-it ()
		    `(bignum-canonicalize
		      (with-inline-assembly (:returns :eax)
			(:compile-two-forms (:eax :ebx) r y)
			(:movzxw (:ebx (:offset movitz-bignum length))
				 :ecx)
			(:leal ((:ecx 1),(* -1 movitz:+movitz-fixnum-factor+))
			       :edx)	; EDX is loop counter
		       xor-loop
			(:movl (:ebx :edx (:offset movitz-bignum bigit0))
			       :ecx)
			(:xorl :ecx (:eax :edx (:offset movitz-bignum bigit0)))
			(:subl 4 :edx)
			(:jnc 'xor-loop)
			))))
	       (do-it)))))))
   (t (&rest integers)
      (declare (dynamic-extent integers))
      (if (null integers)
	  0
	(reduce #'logxor integers)))))

(defun lognot (integer)
  (check-type integer fixnum)
  (with-inline-assembly (:returns :eax)
    (:compile-form (:result-mode :eax) integer)
    (:xorl #.(cl:- #xffffffff movitz::+movitz-fixnum-zmask+) :eax)))

(defun ldb%byte (size position integer)
  "This is LDB with explicit byte-size and position parameters."
  (check-type size positive-fixnum)
  (check-type position positive-fixnum)
  (etypecase integer
    (fixnum
     (macrolet
	 ((do-it ()
	    `(with-inline-assembly (:returns :eax)
	       (:compile-two-forms (:eax :ecx) integer position)
	       (:cmpl ,(* (1- movitz:+movitz-fixnum-bits+) movitz:+movitz-fixnum-factor+)
		      :ecx)
	       (:ja '(:sub-program (outside-fixnum)
		      (:addl #x80000000 :eax) ; sign into carry
		      (:sbbl :ecx :ecx)
		      (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
		      (:jmp 'mask-fixnum)))
	       (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:std)			; <================= STD
	       (:sarl :cl :eax)		; shift..
	       (:andl ,(logxor #xffffffff movitz:+movitz-fixnum-zmask+) :eax)
	       (:cld)			; =================> CLD
	      mask-fixnum
	       (:compile-form (:result-mode :ecx) size)
	       (:cmpl ,(* (1- movitz:+movitz-fixnum-bits+) movitz:+movitz-fixnum-factor+)
		      :ecx)
	       (:jna 'fixnum-result)
	       (:testl :eax :eax)
	       (:jns 'fixnum-done)
	       ;; We need to generate a bignum..
	       ;; ..filling in 1-bits since the integer is negative.
	       (:pushl :eax)		; This will become the LSB bigit.
	      retry-ones-expanded-bignum
	       (:declare-label-set retry-jumper-ones-expanded-bignum (retry-ones-expanded-bignum))
	       ;; Calculate word-size from bytespec-size.
	       (:compile-form (:result-mode :ecx) size)
	       (:addl ,(* 31 movitz:+movitz-fixnum-factor+) :ecx) ; Add 31
	       (:shrl 5 :ecx)		; Divide by 32
	       (:andl ,(- movitz:+movitz-fixnum-factor+) :ecx)
	       (:leal (:ecx ,movitz:+movitz-fixnum-factor+) ; Add 1 for header.
		      :eax)
	       (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
	       (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
				  'retry-jumper-ones-expanded-bignum)
				(:edi (:edi-offset atomically-status))))
	       (:call-local-pf get-cons-pointer)
	       (:shll 16 :ecx)
	       (:orl ,(movitz:tag :bignum 0) :ecx)
	       (:movl :ecx (:eax ,movitz:+other-type-offset+))
	       (:shrl 16 :ecx)
	       (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)
		       ,(* 1 movitz:+movitz-fixnum-factor+)) ; add 1 for header.
		      :ecx)
	       (:call-local-pf cons-commit)
	       (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				(:edi (:edi-offset atomically-status))))
	       ;; Have fresh bignum in EAX, now fill it with ones.
	       (:xorl :ecx :ecx)	; counter
	      fill-ones-loop
	       (:movl #xffffffff (:eax :ecx (:offset movitz-bignum bigit0)))
	       (:addl 4 :ecx)
	       (:cmpw :cx (:eax (:offset movitz-bignum length)))
	       (:jne 'fill-ones-loop)
	       
	       (:popl :ecx)		; The LSB bigit.
	       (:sarl ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:movl :ecx (:eax (:offset movitz-bignum bigit0)))
	       (:movl :eax :ebx)
	       ;; Compute MSB bigit mask in EDX
	       (:compile-form (:result-mode :ecx) size)
	       (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:std)			; <================= STD
	       (:xorl :edx :edx)
	       (:andl 31 :ecx)
	       (:jz 'fixnum-mask-ok)
	       (:addl 1 :edx)
	       (:shll :cl :edx)
	      fixnum-mask-ok
	       (:subl 1 :edx)
	       (:movzxw (:ebx (:offset movitz-bignum length))
			:ecx)
	       (:andl :edx		; And EDX with the MSB bigit.
		      (:ebx :ecx (:offset movitz-bignum bigit0 -4)))
	       (:movl :edi :edx)
	       (:movl :edi :eax)
	       (:cld)			; =================> CLD
	       (:movl :ebx :eax)
	       (:jmp 'fixnum-done)
	       
	      fixnum-result
	       (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
	       (:movl ,movitz:+movitz-fixnum-factor+ :edx) ; generate fixnum mask in EDX
	       (:shll :cl :edx)
	       (:subl ,movitz:+movitz-fixnum-factor+ :edx)
	       (:andl :edx :eax)
	       (:jmp 'fixnum-done)
	      fixnum-done
	       )))
       (do-it)))
    (positive-bignum
     (cond
      ((= size 0) 0)
      ((<= size 32)
       ;; The result is likely to be a fixnum (or at least an u32), due to byte-size.
       (macrolet
	   ((do-it ()
	      `(with-inline-assembly (:returns :eax)
		 (:compile-form (:result-mode :ebx) integer)
		 (:compile-form (:result-mode :eax) position)
		 (:movl :eax :ecx)	; compute bigit-number in ecx
		 (:sarl 5 :ecx)
		 (:andl -4 :ecx)
		 (:addl 4 :ecx)
		 (:cmpl #x4000 :ecx)
		 (:jae 'position-outside-integer)
		 (:cmpw :cx (:ebx (:offset movitz-bignum length)))
		 (:jc '(:sub-program (position-outside-integer)
			(:movsxb (:ebx (:offset movitz-bignum sign)) :ecx)
			(:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
			(:jmp 'done-u32)))
		 (:std)
		 (:movl (:ebx :ecx (:offset movitz-bignum bigit0 -4))
			:eax)
		 (:movl 0 :edx)		; If position was in last bigit.. (don't touch EFLAGS)
		 (:je 'no-top-bigit)	; ..we must zero-extend rather than read top bigit.
		 (:movl (:ebx :ecx (:offset movitz-bignum bigit0))
			:edx)		; Read top bigit into EDX
		no-top-bigit
		 (:testl #xff00 (:ebx ,movitz:+other-type-offset+))
		 (:jnz '(:sub-program (negative-bignum)
			 ;; We must negate the bigits..
			 (:break)
			 ))
		edx-eax-ok
		 ;; EDX:EAX now holds the number that must be shifted and masked.
		 (:compile-form (:result-mode :ecx) position)
		 (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		 (:shrdl :cl :edx :eax)	; Shifted value into EAX
		 (:compile-form (:result-mode :ecx) size)
		 (:xorl :edx :edx)	; Generate a mask in EDX
		 (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		 (:testl 31 :ecx)
		 (:jz 'mask-ok-u32)
		 (:addl 1 :edx)
		 (:shll :cl :edx)
		mask-ok-u32
		 (:subl 1 :edx)
		 (:andl :edx :eax)
		 (:movl :eax :ecx)	; For boxing..
		 (:movl :edi :eax)
		 (:movl :edi :edx)
		 (:cld)
		 ;; See if we can return same bignum..
		 (:cmpl ,(dpb movitz:+movitz-fixnum-factor+
			      (byte 16 16) (movitz:tag :bignum 0))
			(:ebx ,movitz:+other-type-offset+))			     
		 (:jne 'cant-return-same)
		 (:cmpl :ecx (:ebx (:offset movitz-bignum bigit0)))
		 (:jne 'cant-return-same)
		 (:movl :ebx :eax)
		 (:jmp 'done-u32)
		cant-return-same
		 (:call-local-pf box-u32-ecx)
		done-u32
		 )))
	 (do-it)))
      (t (macrolet
	     ((do-it ()
		`(let ()
		   (with-inline-assembly (:returns :eax)
		     (:compile-form (:result-mode :ebx) integer)
		     (:compile-form (:result-mode :ecx) position)
		     (:shrl 5 :ecx) ; compute fixnum bigit-number in ecx
		     (:cmpl #x4000 :ecx)
		     (:jnc 'position-outside-integer)
		     (:cmpw :cx (:ebx (:offset movitz-bignum length)))
		     (:jbe '(:sub-program (position-outside-integer)
			     (:movsxb (:ebx (:offset movitz-bignum sign)) :ecx)
			     (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
			     (:jmp 'done-u32)))
		     
		     (:compile-two-forms (:edx :ecx) position size)
		     (:movl :ecx :eax)	; keep size/fixnum in EAX.
		     (:addl :edx :ecx)
		     (:into)		; just to make sure
		     (:shrl 5 :ecx)	; compute msb bigit index/fixnum in ecx
		     (:addl 4 :ecx)
		     (:cmpw :cx (:ebx (:offset movitz-bignum length)))
		     (je '(:sub-program (equal-size-maybe-return-same)
			   (:testl :edx :edx) ; Can only return same if (zerop position).
			   (:jnz 'adjust-size)
			   (:movl :eax :ecx) ; size/fixnum
			   (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
			   (:andl 31 :ecx)
			   (:jz 'yes-return-same)
			   (:std)	; <================
			   ;; we know EDX=0, now generate mask in EDX
			   (:addl 1 :edx)
			   (:shll :cl :edx)
			   (:movzxw (:ebx (:offset movitz-bignum length))
			    :ecx)
			   (:cmpl :edx (:ebx :ecx (:offset movitz-bignum bigit0 -4)))
			   (:movl 0 :edx) ; Safe value, and correct if we need to go to adjust-size.
			   (:cld)	; =================>
			   (:jnc 'adjust-size) ; nope, we have to generate a new bignum.
			   yes-return-same
			   (:movl :ebx :eax) ; yep, we can return same bignum.
			   (:jmp 'ldb-done)))
		     (:jnc 'size-ok)
		     ;; We now know that (+ size position) is beyond the size of the bignum.
		     ;; So, if (zerop position), we can return the bignum as our result.
		     (:testl :edx :edx)
		     (:jz '(:sub-program ()
			    (:movl :ebx :eax) ; return the source bignum.
			    (:jmp 'ldb-done)))
		    adjust-size
		     ;; The bytespec is (partially) outside source-integer, so we make the
		     ;; size smaller before proceeding. new-size = (- source-int-length position)
		     (:movzxw (:ebx (:offset movitz-bignum length))
			      :ecx)	; length of source-integer
		     (:shll 5 :ecx)	; fixnum bit-position
		     (:xorl :eax :eax)	; In case the new size is zero.
		     (:subl :edx :ecx)	; subtract position
		     (:js '(:sub-program (should-not-happen)
			    ;; new size should never be negative.
			    (:break)))
		     (:jz 'ldb-done)	; New size was zero, so the result of ldb is zero.
		     (:movl :ecx :eax)	; New size into EAX.
		    size-ok
		    retry
		     (:declare-label-set retry-jumper (retry))
		     (:locally (:movl :esp (:edi (:edi-offset atomically-esp))))
		     (:locally (:movl '(:funcall ,(movitz::atomically-status-jumper-fn t :esp)
					'retry-jumper)
				      (:edi (:edi-offset atomically-status))))
		     ;; (new) Size is in EAX.
		     (:pushl :eax)	; Save for later
		     (:subl ,movitz:+movitz-fixnum-factor+ :eax)
		     (:andl ,(logxor #xffffffff
				     (mask-field (byte (+ 5 movitz:+movitz-fixnum-shift+) 0) -1))
			    :eax)
		     (:shrl 5 :eax)	; Divide (size-1) by 32 to get number of bigits-1
		     ;; Now add 1 for index->size, 1 for header, and 1 for tmp storage before shift.
		     (:addl ,(* 3 movitz:+movitz-fixnum-factor+) :eax)
		     (:pushl :eax)
		     (:call-local-pf get-cons-pointer)
		     ;; (:store-lexical (:lexical-binding r) :eax :type t)
		     (:popl :ecx)
		     (:subl ,(* 2 movitz:+movitz-fixnum-factor+) :ecx) ; for tmp storage and header.
		     (:shll 16 :ecx)
		     (:orl ,(movitz:tag :bignum 0) :ecx)
		     (:movl :ecx (:eax ,movitz:+other-type-offset+))
		     (:compile-form (:result-mode :ebx) integer)
		     
		     (:xchgl :eax :ebx)
		     ;; now: EAX = old integer, EBX = new result bignum
		     
		     ;; Edge case: When size(old)=size(new), the tail-tmp must be zero.
		     ;; We check here, setting the tail-tmp to a mask for and-ing below.
		     (:movzxw (:ebx (:offset movitz-bignum length))
			      :ecx)	; length of source-integer
		     ;; Initialize tail-tmp to #xffffffff, meaning copy from source-integer.
		     (:movl #xffffffff (:ebx :ecx (:offset movitz-bignum bigit0)))
		     (:cmpw :cx (:eax (:offset movitz-bignum length)))
		     (:jc '(:sub-program (result-too-big-shouldnt-happen)
			    (:int 4)))
		     (:jne 'tail-tmp-ok)
		     ;; Sizes was equal, so set tail-tmp to zero.
		     (:movl 0 (:ebx :ecx (:offset movitz-bignum bigit0)))
		    tail-tmp-ok
		     ;; Now copy the relevant part of the integer
		     (:std)
		     (:compile-form (:result-mode :ecx) position)
		     (:sarl ,(+ 5 movitz:+movitz-fixnum-shift+) :ecx) ; compute bigit-number in ecx
		     ;; We can use primitive pointers because we're both inside atomically and std.
		     (:leal (:eax (:ecx 4) (:offset movitz-bignum bigit0))
			    :eax)	; Use EAX as primitive pointer into source
		     (:xorl :ecx :ecx)	; counter
		    copy-integer
		     (:movl (:eax) :edx)
		     (:addl 4 :eax)
		     (:movl :edx (:ebx :ecx (:offset movitz-bignum bigit0)))
		     (:addl 4 :ecx)
		     (:cmpw :cx (:ebx (:offset movitz-bignum length)))
		     (:jne 'copy-integer)
		     ;; Copy one more than the length, namely the tmp at the end.
		     ;; Tail-tmp was initialized to a bit-mask above.
		     (:movl (:eax) :edx)
		     (:andl :edx (:ebx :ecx (:offset movitz-bignum bigit0)))
		     ;; Copy done, now shift
		     (:compile-form (:result-mode :ecx) position)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:andl 31 :ecx)
		     (:jz 'shift-done)	; if (zerop (mod position 32)), no shift needed.
		     (:xorl :edx :edx)	; counter
		    shift-loop
		     (:movl (:ebx :edx (:offset movitz-bignum bigit0 4))
			    :eax)	; Next bigit into eax
		     (:shrdl :cl :eax	; Now shift bigit, with msbs from eax.
			     (:ebx :edx (:offset movitz-bignum bigit0)))
		     (:addl 4 :edx)
		     (:cmpw :dx (:ebx (:offset movitz-bignum length)))
		     (:jne 'shift-loop)
		    shift-done
		     ;; Now we must mask MSB bigit.
		     (:movzxw (:ebx (:offset movitz-bignum length))
			      :edx)
		     (:popl :ecx)	; (new) bytespec size
		     (:load-lexical (:lexical-binding size) :ecx)
		     (:shrl 5 :ecx)
		     (:andl -4 :ecx)	; ECX = index of (conceptual) MSB
		     (:cmpl :ecx :edx)
		     (:jbe 'mask-done)
		     
		     (:load-lexical (:lexical-binding size) :ecx)
		     (:shrl ,movitz:+movitz-fixnum-shift+ :ecx)
		     (:andl 31 :ecx)
		     (:jz 'mask-done)
		     (:movl 1 :eax)	; Generate mask in EAX
		     (:shll :cl :eax)
		     (:subl 1 :eax)
		     (:andl :eax (:ebx :edx (:offset movitz-bignum bigit0 -4)))
		    mask-done
		     ;; (:movl :edi :edx)	; safe EDX
		     (:movl :edi :eax)	; safe EAX
		     (:cld)
		     ;; Now we must zero-truncate the result bignum in EBX.
		     (:movzxw (:ebx (:offset movitz-bignum length))
			      :ecx)
		    zero-truncate-loop
		     (:cmpl 0 (:ebx :ecx (:offset movitz-bignum bigit0 -4)))
		     (:jne 'zero-truncate-done)
		     (:subl 4 :ecx)
		     (:jnz 'zero-truncate-loop)
		     ;; Zero bigits means the entire result collapsed to zero.
		     (:xorl :eax :eax)
		     (:jmp 'return-fixnum) ; don't commit the bignum allocation.
		    zero-truncate-done
		     (:cmpl 4 :ecx)	; If result size is 1, the result might have..
		     (:jne 'complete-bignum-allocation) ; ..collapsed to a fixnum.
		     (:cmpl ,movitz:+movitz-most-positive-fixnum+
			    (:ebx (:offset movitz-bignum bigit0)))
		     (:ja 'complete-bignum-allocation)
		     (:movl (:ebx (:offset movitz-bignum bigit0))
			    :ecx)
		     (:leal ((:ecx ,movitz:+movitz-fixnum-factor+)) :eax)
		     (:jmp 'return-fixnum)
		    complete-bignum-allocation
		     (:movw :cx (:ebx (:offset movitz-bignum length)))
		     (:movl :ebx :eax)
		     (:leal (:ecx ,movitz:+movitz-fixnum-factor+)
			    :ecx)
		     (:call-local-pf cons-commit)
		    return-fixnum
		     (:locally (:movl ,(bt:enum-value 'movitz::atomically-status :inactive)
				      (:edi (:edi-offset atomically-status))))
		    ldb-done))))
	   (do-it)))))))

(defun ldb (bytespec integer)
  (ldb%byte (byte-size bytespec) (byte-position bytespec) integer))

(defun ldb-test (bytespec integer)
  (case (byte-size bytespec)
    (0 nil)
    (1 (logbitp (byte-position bytespec) integer))
    (t (/= 0 (ldb bytespec integer)))))

(defun logtest (integer-1 integer-2)
  "=> generalized-boolean"
  (not (= 0 (logand integer-1 integer-2))))

(defun dpb (newbyte bytespec integer)
  (logior (mask-field bytespec (ash newbyte (byte-position bytespec)))
	  (logandc2 integer (mask-field bytespec -1))))

(defun mask-field (bytespec integer)
  (ash (ldb bytespec integer) (byte-position bytespec)))

(defun deposit-field (newbyte bytespec integer)
  (logior (mask-field bytespec newbyte)
	  (logandc2 integer (mask-field bytespec -1))))

;;;

(defun plus-if (x y)
  (if (integerp x) (+ x y) x))

(defun minus-if (x y)
  (if (integerp x) (- x y) x))

(defun gcd (&rest numbers)
  (numargs-case
   (1 (u) u)
   (2 (u v)
      ;; Code borrowed from CMUCL.
      (do ((k 0 (1+ k))
	   (u (abs u) (truncate u 2))
	   (v (abs v) (truncate v 2)))
	  ((or (oddp u) (oddp v))
	   (do ((temp (if (oddp u)
			  (- v)
			(truncate u 2))
		      (truncate temp 2)))
	       (nil)
	     (when (oddp temp)
	       (if (plusp temp)
		   (setq u temp)
		 (setq v (- temp)))
	       (setq temp (- u v))
	       (when (zerop temp)
		 (return (ash u k))))))))
   (t (&rest numbers)
      (declare (dynamic-extent numbers))
      (do ((gcd (car numbers)
		(gcd gcd (car rest)))
	   (rest (cdr numbers) (cdr rest)))
	  ((null rest) gcd)))))

(defun floor (n &optional (divisor 1))
  "This is floor written in terms of truncate."
  (numargs-case
   (1 (n) n)
   (2 (n divisor)
      (multiple-value-bind (q r)
	  (truncate n divisor)
	(cond
	 ((<= 0 q)
	  (values q r))
	 ((= 0 r)
	  (values q 0))
	 (t (values (1- q) (+ r divisor))))))
   (t (n &optional (divisor 1))
      (floor n divisor))))

(defun isqrt (natural)
  "=> natural-root"
  (etypecase natural
    ((eql 0) 0)
    ((integer 1 *)
     (let ((r 1))
       (do ((next-r (truncate (+ r (truncate natural r)) 2)
		    (truncate (+ r (truncate natural r)) 2)))
	   ((typep (- next-r r) '(integer 0 1))
	    (let ((r+1 (1+ r)))
	      (if (<= (* r+1 r+1) natural)
		  r+1
		r)))
	 (setf r next-r))))))

(defun expt (base-number power-number)
  "Take base-number to the power-number."
  (do ((i 0 (1+ i))
       (r 1 (* r base-number)))
      ((>= i power-number) r)))
  
