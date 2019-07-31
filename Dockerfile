FROM haskell:8.0

WORKDIR /app
ADD stack.yaml stack.yaml
ADD package.yaml package.yaml
RUN stack install --only-dependencies

ADD app app
ADD src src
RUN stack build

ENTRYPOINT ["./.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/haskellbot-exe/haskellbot-exe"]
