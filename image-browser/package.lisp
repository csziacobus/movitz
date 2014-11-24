;;; package.lisp

(defpackage movitz-browser
  (:use #:clim
        #:clim-lisp
        #:movitz
        #:binary-types)
  (:export #:browse-file
	   #:browse-pid
	   #:browse-word
	   #:browser))
