# Crypto Trading Terminal

A terminal-based cryptocurrency trading dashboard built with OCaml.

## Features
- Real-time cryptocurrency price monitoring
- Interactive price charts with multiple timeframes
- Market overview with multiple pairs
- Keyboard-based navigation
- Clean TUI (Terminal User Interface)

## Dependencies
- OCaml >= 4.13.0
- opam packages:
  - dune
  - lwt
  - cohttp-lwt-unix
  - yojson
  - notty
  - notty.unix

## Installation

```bash
# Install dependencies
opam install . --deps-only

# Build project
dune build

# Run the terminal
dune exec trading_terminal
```

## Usage
- Tab: Switch between chart and market overview
- Arrow keys: Navigate market pairs
- 1/4/d/w: Change timeframe (1H/4H/1D/1W)
- r: Refresh data
- q: Quit

## Configuration
Edit `lib/market_data.ml` to modify the default trading pairs.


