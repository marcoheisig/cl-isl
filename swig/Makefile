INCLUDES := \
 -I/usr/include/i386-linux-gnu \
 -I/usr/include/x86_64-linux-gnu \
 -I/usr/lib/gcc/x86_64-linux-gnu/9/include \
 -I/usr/lib/gcc/i386-linux-gnu/9/include \
 -I/usr/include

isl.lisp: isl.swig
	swig3.0 -cffi $(INCLUDES) -noswig-lisp -module isl $^

all: isl.lisp
