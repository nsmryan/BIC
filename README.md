BIC
===

Binary to CSV, CSV to Binary

This project provides a way to translate between CSV files and binary files.
The hope is that it will be a tool to easily create and edit such files for
everyday, real world situations.

This is a problem I often has, and I wanted to build my own tool for it
so I would not have to recreate this kind of code again and again.



Possible future direction:
configuration language for structure definitions, including inserting binary files, CRCs, checksum
language may be haskell dsl, may be lisp, idk
read in and translate multiple formats using intermediate representation
create structures with changing/calcuated values
construct and pipe out structures to file/port/etc- translate between streams
conduit streams file any source, including files. assume repeated structures, assume binary.
