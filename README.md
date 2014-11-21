movitz
======

Movitz: a Common Lisp x86 development platform


Implementations
---------------
The implementation I recommend building movitz with is SBCL. CCL's repl is broken on s-exprs
and CLISP are cannot even build movitz correctly as of now.

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


