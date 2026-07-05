# Roadmap

This project is intentionally small. The current server is useful for local
development records and simple forwarding, but it is not a complete recursive
resolver or authoritative DNS implementation.

Live tracking:

- Project: <https://github.com/users/owainlewis/projects/19>
- Issues: <https://github.com/owainlewis/hdns/issues?q=is%3Aissue%20label%3Amodernization>

## Backlog

- [Expand local zone parsing](https://github.com/owainlewis/hdns/issues/2)
- [Add authoritative negative responses](https://github.com/owainlewis/hdns/issues/3)
- [Add observability and health checks](https://github.com/owainlewis/hdns/issues/4)

## Ready

- [Add TCP listener and fallback handling](https://github.com/owainlewis/hdns/issues/1)
- [Add CI and release packaging](https://github.com/owainlewis/hdns/issues/5)

## In Progress

- Modernize the UDP server into a configurable local records plus forwarding
  server.

## Done

- Add non-root default runtime on UDP port 1053.
- Add local A, AAAA, CNAME, and TXT records.
- Add upstream forwarding for cache misses.
- Add focused DNS message handler tests.
