# aoc2021

Working through [Advent of Code 2021](https://adventofcode.com/2021) in [Haskell](https://www.haskell.org/).

Needed things
---
*   [Nix](https://nixos.org/download.html)

Quick start
---
```bash
$ nix-shell
[nix-shell:path/to/aoc2021]$ for i in $(seq -f "%02g" 1 9); do
[nix-shell:path/to/aoc2021]$     lint "src/Day${i}.hs"
[nix-shell:path/to/aoc2021]$     run "src/Day${i}.hs" "data/${i}_0.txt"
[nix-shell:path/to/aoc2021]$     run "src/Day${i}.hs" "data/${i}_1.txt"
[nix-shell:path/to/aoc2021]$ done
```
