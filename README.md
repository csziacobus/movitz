movitz
======

Movitz: a Common Lisp x86 development platform


Implementations
---------------
The implementation I recommend building movitz with is SBCL. CCL's repl is broken on s-exprs
and CLISP cannot even build movitz correctly as of now.

Getting started with an image
-----------------------------
First, fire up your Lisp implementation and load the `movitz.asd` file.
Afterwards load the system and evaluate the form `(movitz:create-image)`.
When the symbolic image finishes loading you can then dump the image with
`(movitz:dump-image :path "foo.img")`, which will dump the image associated with the variable
`*movitz:image*` to `foo.img`.

Now there are two options:

- Use the image directly
- Concatenate the grub bootloader with `foo.img`; for example,
`cat grub-bootloader/grub-bootloader.img foo.img > hda.img`

You can use `qemu-system-i386 foo.img` to run the image in a virtualized environment.

Documentation and Information
-----------------------------
More information on movitz can be found at the [Movitz Trac Page]
(http://trac.common-lisp.net/movitz/wiki)

Movitz and Slime
----------------
To enable the Movitz extension to Slime, evaluate

  `(add-to-list 'load-path "/path/to/movitz/ide/")`
  `(load-library "movitz-slime")`

You can then use `movitz-mode` in conjunction with slime.
For example, say you have created a symbolic image with
`(movitz:create-image)`; then you can work with the Movitz
source as with any Lisp source.
The commands provided by movitz-mode, such as movitz-compile-defun
(M-C-x), movitz-disassemble-defun (C-c C-v), etc. manipulate the
symbolic image in movitz:*image* (typically the image you last
created with create-image). In order to actually run any code
you compile, you have to re-dump and re-boot (which
movitz-dump-image-and-qemu
(C-c C-d) is supposed to do automatically. If you just want to
dump the image you can use `(C-C d)).

You may need to set the variable movitz-mode-qemu-binary-path
in order to have qemu automatically execute the image.


