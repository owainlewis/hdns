FROM haskell:9.10 AS build

WORKDIR /src
COPY hdns.cabal cabal.project* ./
RUN cabal update
COPY . .
RUN cabal build hdns-exe
RUN mkdir -p /out && cp "$(cabal list-bin hdns-exe)" /out/hdns

FROM debian:bookworm-slim

RUN apt-get update \
  && apt-get install -y --no-install-recommends ca-certificates \
  && rm -rf /var/lib/apt/lists/*

RUN useradd --system --create-home --home-dir /var/lib/hdns hdns
USER hdns

COPY --from=build /out/hdns /usr/local/bin/hdns

EXPOSE 1053/udp
ENTRYPOINT ["/usr/local/bin/hdns"]
CMD ["--bind", "0.0.0.0", "--port", "1053"]
