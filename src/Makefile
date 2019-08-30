shared := libread_ie.so libuinject.so
#mains := test_libs.run

#all:  $(shared) $(mains)
all:  $(shared)

read_ie3.run:
	gcc -o read_ie3.run read_ie3.c

%.run: %.c
	gcc -o $@ $< $(shared)

lib%.so: %.c
	gcc -shared -fPIC -Wl,-soname,$@ -o $@ $<

clean:
	rm -rf *~ $(shared) $(mains)

install: 
	rm -f ../lib/lib*
	cp lib*.so ../lib/

.PHONY: all clean install
