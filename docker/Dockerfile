# Dedicated to my aunt Izzy; I love and miss her a lot.
#
# BUILD: docker build -t debian-izzy .
#
# TEST RUN:
#
# docker run --rm -it -v --name debian-izzy debian-izzy <test-script>
#
# DEV RUN:
#
# docker run --rm -it -v /home/q/izzy:/root/izzy-test/izzy:Z --name
# debian-izzy debian-izzy

FROM debian

RUN apt-get update && \
    apt-get -y install gcc git guile-2.0 make && \
    cd ~ && \
    mkdir izzy-test && \
    cd izzy-test && \
    mkfifo kie-pipe && \
    git clone https://github.com/IkeLewis/izzy.git

ENTRYPOINT [ "/root/izzy-test/docker/test.sh" ]