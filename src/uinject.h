#ifndef _UINJECT_H_
#define _UINJECT_H_

/*Attempts to initialize the library*/
void initUinject();

/*Attempts to inject an input event into the uinput device.  Only key
  events are handled.  All other input events are currently ignored.
  Synchronization events are handled automatically. */
int uinject(struct input_event * ie);

/*Attempts to shutdown the library*/
void shutdownUinject();

#endif /* _UINJECT_H_ */
