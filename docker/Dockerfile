# BUILD: 
#
# $ cd /path-to-izzy-repo/docker
# $ docker build -t debian-izzy .
#
# TEST RUN:
#
# $ docker run --rm -it --name debian-izzy debian-izzy
# 
# DEV RUN:
#
# $ docker run --rm -it -v /path-to-izzy-repo/:/root/izzy:Z --name \
# debian-izzy debian-izzy

FROM debian

RUN apt-get update && \
    apt-get -y install gcc git guile-2.0 guile-2.2 make && \
    cd ~ && \
    git clone https://github.com/IkeLewis/izzy.git && \
    cd /root/izzy/test && \
    mkfifo kie-pipe && \
    touch unix-server-socket

ENTRYPOINT [ "/root/izzy/docker/test.sh" ]