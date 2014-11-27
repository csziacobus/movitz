(in-package #:movitz)

(defvar *movitz-readtable* (copy-readtable))

(defun sharpsign-single-quote (stream char parameter)
  (declare (ignore char))
  (when parameter
    (warn "A numeric argument was ignored in #~a'." parameter))
  `(muerte.common-lisp::function ,(read stream t nil t)))

(defun sharpsign-left-brace (stream char parameter)
  (declare (ignore char))
  (when parameter
    (warn "A numeric argument was ignored in #~a{." parameter))
  (let ((data (read-delimited-list #\} stream)))
    (make-movitz-vector (length data)
                        :element-type 'movitz-unboxed-integer-u8
                        :initial-contents data)))

(defun sharpsign-dot (stream char parameter)
  (declare (ignore char))
  (when parameter
    (warn "A numeric argument was ignored in #~a." parameter))
  (let ((form (read stream t nil t)))
    (values (unless *read-suppress*
	      (eval (muerte::translate-program form :muerte.cl :cl))))))

(defun backquote (stream char)
  (declare (ignore char))
  (let ((*backquote-depth* (1+ *backquote-depth*)))
    `(muerte::movitz-backquote ,(read stream t nil t))))

(defun comma (stream char)
  (declare (ignore char))
  (unless (plusp *backquote-depth*)
    (error "Comma not inside backquote."))
  (let* ((next-char (read-char stream t nil t))
         (comma-type (case next-char
                       (#\@ 'unquote-splicing)
                       (#\. 'unquote-nsplicing)
                       (t (unread-char next-char stream)
                        'unquote))))
    `(,comma-type ,(read stream t nil t))))

(set-dispatch-macro-character #\# #\' #'sharpsign-single-quote *movitz-readtable*)
(set-dispatch-macro-character #\# #\{ #'sharpsign-left-brace *movitz-readtable*)
(set-dispatch-macro-character #\# #\. #'sharpsign-dot *movitz-readtable*)
(set-macro-character #\` #'backquote nil *movitz-readtable*)
(set-macro-character #\, #'comma nil *movitz-readtable*)
