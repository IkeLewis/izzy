# BUILD: docker build -t debian-izzy .

# RUN: docker run --rm -itd --name debian-izzy

FROM debian

RUN apt-get update && \
    apt-get -y install git guile-2.2 && \
    cd && \
    git clone https://github.com/IkeLewis/izzy.git
