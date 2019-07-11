#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* for memset */
#include <linux/input.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>

#define LISTEN_BACKLOG 1


#define handle_error(msg)				\
  do { perror(msg); exit(EXIT_FAILURE); } while (0)


int main() {
  //"io.raw"

  int sfd = socket(AF_UNIX, SOCK_RAW, 0); 

  struct sockaddr_un my_addr;

  my_addr.sun_family = AF_UNIX;
  my_addr.sun_path = "./grr.sock";
  
  if(bind(sfd, my_addr, sizeof(struct sockaddr_un)) == -1)
    handle_error("bind");
  
  if (listen(sfd, LISTEN_BACKLOG) == -1)
    handle_error("listen");

  /* Now we can accept incoming connections one
     at a time using accept(2) */

  peer_addr_size = sizeof(struct sockaddr_un);
  cfd = accept(sfd, (struct sockaddr *) &peer_addr,
	       &peer_addr_size);
  if (cfd == -1)
    handle_error("accept");

  //  int fd = open("/dev/input/by-path/platform-i8042-serio-0-event-kbd", O_RDWR | O_APPEND);

  if(fd == -1)
    printf("%s", "bust");
  
  struct input_event * ie;

  memset(ie, 65, sizeof(struct input_event));
  write(fd, ie, sizeof(struct input_event));
  
  memset(ie, 10, sizeof(struct input_event));
  write(fd, ie, sizeof(struct input_event));

  close(fd);
}
