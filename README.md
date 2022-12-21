# Park Bench

A minimal benchmarking library for idris 2.

The main function this exposes is the `bench` function, which benchmarks a pure function repeatedly until sufficient time has passed (this is configurable). You can then use the default `Show` implementation to output the statistics in a human readable format.
