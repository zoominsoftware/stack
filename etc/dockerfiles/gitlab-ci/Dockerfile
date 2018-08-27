FROM fpco/stack-build:lts-8.24

# Install required Ubuntu packages
RUN ( apt-get update ) \
 && ( DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends mercurial) \
 && ( apt-get clean ) \
 && ( rm -rf /var/lib/apt/lists/* )

# Install latest version of Stack
ARG STACK_VERSION=1.7.1
RUN curl -sSL https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
