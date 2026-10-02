# syntax=docker/dockerfile:1
#
# Imagen de Record System en varias etapas:
#   build   -> compila con GnuCOBOL
#   test    -> ejecuta las pruebas (docker build --target test .)
#   runtime -> imagen final liviana, sólo con la biblioteca libcob

ARG DEBIAN_VERSION=bookworm-slim

FROM debian:${DEBIAN_VERSION} AS build
RUN apt-get update \
    && apt-get install -y --no-install-recommends gnucobol3 make \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY Makefile ./
COPY src ./src
COPY tests ./tests
COPY examples ./examples
COPY scripts ./scripts
RUN make build

FROM build AS test
RUN make test examples

FROM debian:${DEBIAN_VERSION} AS runtime
RUN apt-get update \
    && apt-get install -y --no-install-recommends libcob4 tzdata \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --create-home --uid 1000 recordsystem \
    && mkdir /data \
    && chown recordsystem:recordsystem /data
COPY --from=build /app/bin/record_system /usr/local/bin/record_system
COPY scripts/record-system.sh /usr/local/bin/record-system
ENV RS_DATA_DIR=/data \
    TZ=America/Argentina/Buenos_Aires
USER recordsystem
WORKDIR /data
VOLUME ["/data"]
ENTRYPOINT ["record-system"]
