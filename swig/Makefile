INCLUDES := \
 -I/usr/include/i386-linux-gnu \
 -I/usr/include/x86_64-linux-gnu \
 -I/usr/lib/gcc/x86_64-linux-gnu/9/include \
 -I/usr/lib/gcc/i386-linux-gnu/9/include \
 -I/usr/include

swig-interface.lisp: isl.swig prologue.lisp
	swig3.0 -cffi $(INCLUDES) -noswig-lisp -module isl $<
#       We handle structs, enums, unions and constants with CFFI's groveller
	cat prologue.lisp > swig-interface.lisp
	sed '/^(\(cffi:defcenum\|cffi:defcunion\|cl:defconstant\)/,/^$$/d' isl.lisp >> swig-interface.lisp

all: isl.lisp
