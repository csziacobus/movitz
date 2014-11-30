(in-package #:movitz)

(defparameter *grub-bootloader-path*
  (asdf:system-relative-pathname
   :movitz "grub-bootloader/grub-bootloader.img"))

(defun dump-image-to-file (filename &key (grub-bootloader-p t))
  (let ((path (asdf:system-relative-pathname :movitz
                                             filename)))
    (cond (grub-bootloader-p
           (dump-image :path "/tmp/tmp.img")
           (when grub-bootloader-p
             (with-open-file (out path :direction :output
                                       :if-exists :supersede)
               (uiop:run-program
                `("cat" ,(namestring *grub-bootloader-path*)
                        "/tmp/tmp.img")
                :output out))))
          (t (dump-image :path path)))))

(defun run-image-file (filename)
  (uiop:run-program
   (format nil "qemu-system-i386 ~a"
           (asdf:system-relative-pathname :movitz filename))))
