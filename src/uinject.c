#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <linux/input.h>
#include <linux/uinput.h>

#include "uinject.h"

#define dienr(str, args...) do {		\
    perror(str);				\
    exit(EXIT_FAILURE);				\
  } while(0)


#define die(str, args...) do {			\
    perror(str);				\
    return -1;                         		\
  } while(0)


#define DEBUG 1

int fd;
struct uinput_user_dev uidev;
char initialized = 0;

//sync event issued after a press/release
struct input_event se;
//sync event issued after a repeat
struct input_event rse;  
//right_ctrl misc event
struct input_event rcme;
//g misc event
struct input_event gme;


void initMiscEvents() {
  memset(&rcme, 0, sizeof(struct input_event));
  rcme.type = 4;  
  rcme.code = 4;
  rcme.value = 458980;
  memset(&gme, 0, sizeof(struct input_event));
  gme.type = 4;
  gme.code = 4;
  gme.value = 458762;
}

void initSyncEvents() {
  memset(&se, 0, sizeof(struct input_event));
  se.type = EV_SYN;
  se.code = 0;
  se.value = 0;
  memset(&rse, 0, sizeof(struct input_event));
  rse.type = EV_SYN;
  rse.code = 0;
  rse.value = 1;
}

void initIoctls() {

  /* Allow keyboard events */
  if(ioctl(fd, UI_SET_EVBIT, EV_KEY) < 0)
    dienr("error: ioctl");

  /* Allow synchronization events */
  if(ioctl(fd, UI_SET_EVBIT, EV_SYN) < 0)
    dienr("error: ioctl");

  /* Allow misc events */
  if(ioctl(fd, UI_SET_EVBIT, 2) < 0)
    dienr("error: ioctl");

  int i;
  for(i=0; i<=4; i++) {
    if(ioctl(fd, UI_SET_EVBIT, i) < 0)
      dienr("error: ioctl");
  }  
  
  /*0-83*/
  for(i=KEY_RESERVED+1; i<=KEY_KPDOT; i++) {
    if(ioctl(fd, UI_SET_KEYBIT, i) < 0)
      dienr("error: ioctl");
  }

  /*85-194*/
  for(i=KEY_ZENKAKUHANKAKU; i<=KEY_F24; i++) {
    if(ioctl(fd, UI_SET_KEYBIT, i) < 0)
      dienr("error: ioctl");
  }

  /*200-248*/
  for(i=KEY_PLAYCD; i<=KEY_MICMUTE; i++) {
    if(ioctl(fd, UI_SET_KEYBIT, i) < 0)
      dienr("error: ioctl");
  }

  /*352-442*/
  for(i=BTN_MISC; i<=KEY_IMAGES; i++) {
    if(ioctl(fd, UI_SET_KEYBIT, i) < 0)
      dienr("error: ioctl");
  }

  /* Allow mouse events */
  if(ioctl(fd, UI_SET_EVBIT, EV_REL) < 0)
    dienr("error: ioctl");

  if(ioctl(fd, UI_SET_RELBIT, REL_X) < 0)
    dienr("error: ioctl");

  if(ioctl(fd, UI_SET_RELBIT, REL_Y) < 0)
    dienr("error: ioctl");

}

/* API */
void initUinject() {

  //if(initialized)
  // return;

  fd = open("/dev/uinput", O_WRONLY | O_NONBLOCK);

  if(fd < 0)
    dienr("error: open");

  initSyncEvents();

  initMiscEvents();
  
  /* Specify what input events the device will accept */
  initIoctls();
    
  memset(&uidev, 0, sizeof(uidev));
  snprintf(uidev.name, UINPUT_MAX_NAME_SIZE, "uinput-uinject");
  uidev.id.bustype = BUS_USB;//BUS_XTKBD;
  uidev.id.vendor  = 0x1;
  uidev.id.product = 0x1;
  uidev.id.version = 1;

  if(write(fd, &uidev, sizeof(uidev)) < 0)
    dienr("error: write");

  if(ioctl(fd, UI_DEV_CREATE) < 0)
    dienr("error: ioctl");

  /* Wait for the device to be created */
  usleep(500000);

  initialized = 1;

}

int uinject(struct input_event * ie) {

  if(!initialized)
    initUinject();

  if(DEBUG)
    printf("5) uinject: type: %d, value: %d, code: 0x%04x (%d), secs: %d, usecs: %d\n",
	   ie->type, ie->value, (int)ie->code, (int)ie->code, ie->time.tv_sec, ie->time.tv_usec);

  //only handle key events
  if(ie->type == EV_KEY) {

    if(ie->code == 97 && (ie->value == 1 || ie->value == 0)) {
      printf("%s", "sending right control misc event\n");
      rcme.time = ie->time;
      if(write(fd, &rcme, sizeof(struct input_event)) < 0)
	die("error: write");
    }

    if(ie->code == 34 && (ie->value == 1 || ie->value == 0)) {
      printf("%s", "sending g misc event\n");
      gme.time = ie->time;
      if(write(fd, &gme, sizeof(struct input_event)) < 0)
	die("error: write");
    }
    
    if(write(fd, ie, sizeof(struct input_event)) < 0)
      die("error: write");

    if(DEBUG)
      printf("5) uinject: %s", "syncing\n");
    //if ie is a press or a release
    if (ie->value == 1 || ie->value == 0) {
      //send the sync event se (code: 0, type: 0, value: 0)
      se.time = ie->time;
      if(write(fd, &se, sizeof(struct input_event)) < 0)
	die("error: write");
    }
    //else if ie is a repeat
    else if (ie->value == 2) {
      //send the sync event rse (code: 0, type: 0, value: 1)
      rse.time = ie->time;
      if(write(fd, &rse, sizeof(struct input_event)) < 0)
	die("error: write");
            
    }
  }

  return EXIT_SUCCESS;
}

void shutdownUinject() {

  if(ioctl(fd, UI_DEV_DESTROY) < 0)
    dienr("error: ioctl");

  close(fd);

}
