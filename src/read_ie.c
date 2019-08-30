#include <errno.h>
#include <fcntl.h>
#include <linux/input.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// Compilation command:
// gcc -o readie readie.c

int main(int argc, char *argv[]) {

  if(!(2 <= argc && argc <= 3)) {
    fprintf(stderr, "--------------------------------------------------------------------------------\nUsage: %s <path-to-input-device> [<path-to-output-file>]\n \n Read input events from the given input device and print them to\n standard output.  If an output file is specified, the raw input\n events are written to the file.\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  // Stack allocated variables {
  struct input_event ie;
  size_t sizeofie = sizeof(ie);
  // number of bytes read
  ssize_t n;
  // used to copy the value of errno, an integer
  int errno2;
  // number of bytes written to fdout
  ssize_t nw;
  // flush status
  int fstatus;
  // }

  // Heap allocated variables {
  //
  // File descriptors are closed automatically with a call to exit see
  // the manpage for exit(2).
  //
  // file descriptor for the input device file
  int fd;
  // file descriptor for the output file
  FILE* outfile;
  // }

  fd = open(argv[1], O_RDONLY);

  if(fd == -1) {
    errno2 = errno;
    fprintf(stderr, "open: %s\n", strerror(errno2));
    exit(errno2);
  }

  outfile = NULL;
  if(argc == 3) {
    outfile = fopen(argv[2], "a+");
    if(outfile == NULL) {
      errno2 = errno;
      fprintf(stderr, "open: %s\n", strerror(errno2));
      exit(errno2);
    }
  }

  for(;;) {

    memset(&ie, 0, sizeofie);

  tryread:
    errno = 0;
    n = read(fd, &ie, sizeofie);
    if(n == (ssize_t)(-1)) {
      if(errno == EINTR)
	goto tryread;
      else {
	errno2 = errno;
	fprintf(stderr, "readIE: %s\n", strerror(errno2));
	exit(errno2);
      }
    }
    // Handle partial reads; for now we'll consider them to be an
    // error.
    else if(n != sizeofie) {
      errno = EIO;
      errno2 = errno;
      fprintf(stderr, "readIE: partial read: %s\n", strerror(errno2));
      exit(errno2);
    }

    printf("readie: code %i, type: %i, value: %i, secs: %i, usecs: %i, sizeofie: %i\n",
	   ie.code, ie.type, ie.value, ie.time.tv_sec, ie.time.tv_usec, sizeof(ie));

    // if an out file was specified on the command line
    if(outfile != NULL) {
      nw = fwrite(&ie, sizeof(ie), 1, outfile);

      if(nw == (ssize_t)(-1)){
	errno2 = errno;
	fprintf(stderr, "write: %s\n", strerror(errno2));
	exit(errno2);
      }
      // Technically, a partial write may not be an error, e.g. if the
      // disk is full.  For simplicity, however, we'll regard them as
      // an error.
      if(nw != 1) {
	errno = EIO;
	errno2 = errno;
	fprintf(stderr, "write: partial write: %s\n", strerror(errno2));
	exit(errno2);
      }
      // flush the output right away
      fstatus = fflush(outfile);
      if(fstatus != 0) {
	errno2 = errno;
	fprintf(stderr, "fflush: %s\n", strerror(errno2));
	exit(errno2);
      }
    }

    printf("wrote: %i bytes to outfile \n", nw * sizeof(ie));
  }

  return 0;

}

// 
