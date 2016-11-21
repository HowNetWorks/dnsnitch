FROM haskell:8.0

RUN useradd -m app && useradd -m build

WORKDIR /build
COPY dnsnitch.cabal stack.yaml ./
RUN chown -R build:build /build
USER build
RUN stack install --dependencies-only

USER root
COPY . .
RUN chown -R build:build /build
USER build
RUN stack install

USER root
WORKDIR /app
RUN cp /home/build/.local/bin/dnsnitch /app/
USER app
EXPOSE 1053 8080
ENTRYPOINT ["/app/dnsnitch", "1053", "8080"]
