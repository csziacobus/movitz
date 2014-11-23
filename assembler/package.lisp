;; pacakge.lisp

(defpackage asm
  (:use :common-lisp)
  (:export #:symbol-reference-p
	   #:symbol-reference
	   #:symbol-reference-symbol
	   #:immediate-p
	   #:immediate-operand
	   #:indirect-operand-p
	   #:indirect-operand
	   #:indirect-operand-offset
	   #:instruction-operands
	   #:instruction-operator
	   #:register-operand
	   #:resolve-operand
	   #:unresolved-symbol
	   #:retry-symbol-resolve
	   #:pc-relative-operand
	   #:assemble-proglist
	   #:disassemble-proglist
	   #:*pc*
	   #:*symtab*
	   #:*instruction-compute-extra-prefix-map*
	   #:*position-independent-p*
	   #:*sub-program-instructions*))
