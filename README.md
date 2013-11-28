BIC
===

Binary to CSV, CSV to Binary

This project provides a way to translate between CSV files and binary files.
The hope is that it will be a tool to easily create and edit such files for
everyday, real world situations.

This is a problem I often have, and I wanted to build my own tool for it
so I would not have to recreate this kind of funcationality again and again.


currently:
csv -> [val] -> binary
binary -> [val] -> csv
from file to file

ideally- format -> repr and repr -> format'
from source -> sink of different kinds


Possible future direction:
configuration language for structure definitions, including inserting binary files, CRCs, checksum
language may be haskell dsl, may be lisp, idk
read in and translate multiple formats using intermediate representation
create structures with changing/calcuated values
construct and pipe out structures to file/port/etc- translate between streams
conduit streams file any source, including files. assume repeated structures, assume binary.


how would this look in Forth?
3 muxInfo 4 subcom 3 pad0 0 pad1 crc
