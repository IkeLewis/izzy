#include "read_ie.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* for memset */

int readIE(int fd, struct input_event * ie) {

  memset(ie, 0, sizeof(struct input_event));
  ssize_t n;

 tryread:
  errno = 0;
  n = read(fd, ie, sizeof(struct input_event));
  if (n == (ssize_t)(-1)) {
    if (errno == EINTR)
      goto tryread;
    else {
      fprintf(stderr, "readIE: %s\n", strerror(errno));
      return -1;
    }
  } else
    if (n != sizeof(struct input_event)) {
      //It may not be appropriate to consider partial reads to be an error
      errno = EIO;
      fprintf(stderr, "readIE (partial read error): %s\n", strerror(errno));
      return -1;
    }

  return 0;

}
