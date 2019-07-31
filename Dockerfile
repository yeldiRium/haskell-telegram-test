FROM haskell:8.0

WORKDIR /app
ADD stack.yaml stack.yaml
ADD package.yaml package.yaml
RUN stack install --only-dependencies

ADD . /app
RUN stack build

CMD ["./.stack-cork/dist/x86_64-linux/Cabal-1.24.2.0/build/testbot-exe/testbot-exe"]
