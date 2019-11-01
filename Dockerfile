FROM ubuntu AS BASE

RUN apt-get update
RUN apt-get install -yy curl unzip build-essential zlib1g-dev
WORKDIR "/opt"
RUN curl -sLO https://github.com/oracle/graal/releases/download/vm-19.2.1/graalvm-ce-linux-amd64-19.2.1.tar.gz
RUN tar -xzf graalvm-ce-linux-amd64-19.2.1.tar.gz
ENV GRAALVM_HOME="/opt/graalvm-ce-19.2.1"
ENV JAVA_HOME="/opt/graalvm-ce-19.2.1/bin"
ENV PATH="$PATH:$JAVA_HOME"
COPY . .
RUN apt install -y sudo
RUN ./.circleci/script/install-leiningen
RUN ./script/compile
RUN cp clj-kondo /usr/local/bin


FROM alpine:3.9

# See https://github.com/sgerrand/alpine-pkg-glibc
RUN apk --no-cache add ca-certificates curl
RUN curl -o /etc/apk/keys/sgerrand.rsa.pub -sL https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub
RUN curl -sLO https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.29-r0/glibc-2.29-r0.apk
RUN apk add glibc-2.29-r0.apk
COPY --from=BASE /usr/local/bin/clj-kondo /usr/local/bin
ENV LD_LIBRARY_PATH /lib
CMD ["clj-kondo"]
