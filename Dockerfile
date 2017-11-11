FROM ubuntu:latest

# get dependencies
RUN apt-get update
# compilers
RUN apt-get install -y build-essential g++-4.7-arm-linux-gnueabi
# git & gem5 deps
RUN apt-get install -y git-core m4 scons swig python-dev python zlib1g zlib1g-dev
# protobuf dep
RUN apt-get install -y libprotobuf-dev python-protobuf protobuf-compiler libgoogle-perftools-dev
RUN apt-get clean

# checkout gem5
WORKDIR /usr/local/src
RUN git clone https://gem5.googlesource.com/public/gem5
# build it
WORKDIR /usr/local/src/gem5
# replace string
RUN sed -i "s/3.0.0/3.3.0/" src/arch/arm/linux/process.cc
RUN scons --ignore-style build/ARM/gem5.opt

# replace previous binary version with newly built gem5
RUN rm -f /usr/local/bin/gem5.opt
RUN mv build/ARM/gem5.opt /usr/local/bin
RUN rm -rf build
RUN mkdir -p build/ARM
RUN ln -s /usr/local/bin/gem5.opt build/ARM/gem5.opt

WORKDIR /usr/local/src/pja3
