#ifndef _READIE_H_
#define _READIE_H_

#include <linux/input.h>

/* Attempts to use the file descriptor fd read the next input event
   into the "out" argument ie. Returns -1 on error or 0 on success. */
int readIE(int fd, struct input_event * ie);

#endif /* _READIE_H_ */
