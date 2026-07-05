# hdns

`hdns` is a small DNS server written in Haskell. It serves local records from a
plain text zone file and can forward misses to an upstream resolver.

Status: alpha. The server is useful for local development and small controlled
deployments. It is not a complete recursive resolver and it does not implement
authoritative DNS zone transfers.

## Features

- UDP DNS server with configurable bind host and port.
- Safe non-root default: `127.0.0.1:1053`.
- Local A, AAAA, CNAME, and TXT records.
- Optional forwarding to an upstream resolver.
- Clear DNS responses for malformed packets, unsupported operations, local
  misses, upstream failures, and oversized UDP responses.
- Focused tests for record parsing and DNS message behavior.
- Docker image that runs as an unprivileged user.

## Build And Test

```sh
cabal update
cabal build all
cabal test all
```

Stack users can also run:

```sh
stack build
stack test
```

This repository now checks in `hdns.cabal` so Cabal works without requiring
`hpack`.

## Run Locally

Create or edit a records file:

```zone
# name ttl type value
example.test. 60 A 192.0.2.10
example.test. 60 AAAA 2001:db8::10
alias.example.test. 60 CNAME example.test.
text.example.test. 60 TXT "hello from hdns"
```

Start the server:

```sh
cabal run hdns-exe -- \
  --bind 127.0.0.1 \
  --port 1053 \
  --records examples/records.zone \
  --upstream 1.1.1.1
```

Query local records:

```sh
dig @127.0.0.1 -p 1053 example.test A
dig @127.0.0.1 -p 1053 example.test AAAA
dig @127.0.0.1 -p 1053 text.example.test TXT
```

Query through the upstream resolver:

```sh
dig @127.0.0.1 -p 1053 www.example.com A
```

## Configuration

`hdns-exe` accepts these options:

```text
--bind HOST             Bind host. Default: 127.0.0.1
--port PORT             UDP port. Default: 1053
--upstream HOST         Forward misses to this resolver
--upstream-port PORT    Upstream resolver port. Default: 53
--no-forward            Disable forwarding
--records FILE          Local records file
--log-level LEVEL       quiet, info, or debug. Default: info
--max-udp-size BYTES    Server UDP response limit. Default: 1232
```

Record lines use:

```text
name ttl type value
```

The TTL is optional. If omitted, it defaults to 300 seconds:

```zone
example.test. A 192.0.2.10
```

Supported local record types are `A`, `AAAA`, `CNAME`, and `TXT`.

## Docker

Build the image:

```sh
docker build -t hdns .
```

Run on a non-root port:

```sh
docker run --rm \
  -p 1053:1053/udp \
  -v "$PWD/examples/records.zone:/records.zone:ro" \
  hdns --bind 0.0.0.0 --port 1053 --records /records.zone --upstream 1.1.1.1
```

To run on port 53 in production, bind the container or service explicitly to
UDP 53 and keep the server configuration visible:

```sh
docker run --rm \
  -p 53:1053/udp \
  -v "$PWD/examples/records.zone:/records.zone:ro" \
  hdns --bind 0.0.0.0 --port 1053 --records /records.zone --upstream 1.1.1.1
```

## Behavior

- Local records always win over forwarding.
- If a local name exists but the requested type does not, `hdns` returns
  `NOERROR` with an empty answer section.
- If a local name does not exist and forwarding is disabled, `hdns` returns
  `NXDOMAIN`.
- If upstream forwarding fails, `hdns` returns `SERVFAIL`.
- Non-standard opcodes and UDP zone transfers return `NOTIMP`.
- Malformed packets return `FORMERR` when enough of the packet is present to
  send a response.
- UDP responses are capped by the lower of the server limit and the client EDNS
  limit. Oversized responses are marked truncated.

## Architecture

- `Network.HDNS.Config` parses command-line options and local records.
- `Network.HDNS.Handler` contains pure and testable DNS message behavior.
- `Network.HDNS.Server` owns sockets, concurrency, forwarding, and shutdown.

Each UDP request is handled in a lightweight thread. Local records are immutable
after startup. The upstream resolver comes from the `dns` package and is used in
its concurrent mode.

## Limitations

- UDP only. There is no TCP fallback listener yet.
- No DNSSEC validation.
- No recursive resolver implementation.
- No wildcard records.
- No zone-file `$ORIGIN`, `$TTL`, includes, or escaped TXT parsing.
- No SOA authority records for negative local responses.

## Roadmap

See [docs/ROADMAP.md](docs/ROADMAP.md).
