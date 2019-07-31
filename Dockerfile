FROM haskell:8

WORKDIR /app
ADD . /app
RUN stack build
