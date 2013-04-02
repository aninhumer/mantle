% Mantle: Building HDL abstractions in Haskell
% Alex Horsman
% 7^th^ February 2013

# The Problem

- Verilog and VHDL
    - No type checking
    - Limited abstraction
    - Designed for simulation not synthesis
- Bluespec
    - Transaction model
    - Low level optimisation difficult
        - Hard to reason about performance
        - Some constructs impossible
    - Haskell abstraction features removed
    - Commercial and closed source


# Solution

- Low level RTL model in Haskell
    - Constructed with an embedded DSL
    - Compiles to synthesisable Verilog
- New abstractions layered on top
    - Different components use different abstractions
    - Structured wire groups as common interface

# Work So Far

- RTL model and low level DSL complete
- Generates valid Verilog
- Interface abstraction working
    - Slightly inelegant
- Some higher level abstractions
    - Synchronous circuits
    - Guarded channels

# Synchronous Example

``` haskell
counter :: SyncComp (Output Int)
counter out = do
    count <- reg 0
    onClock $ do
        count <=: rd count + 1
    out =: rd count
```

# Channels Example

``` haskell
fifoChain :: (Bits a) => SyncComp (FIFO a)
fifoChain (Pipe inchan outchan) = do
    fifoA :: FIFO a <- make fifo
    fifoB :: FIFO a <- make fifo
    inchan >-> fifoA
    fifoA  >-> fifoB
    fifoB  >-> outchan
```

# Further Work

- Lots more examples
    - Various FIFOs
    - Sorting network
    - DSP algorithms
    - Mandelbrot render
- Possible extensions
    - Imperative DSL for state machines
    - Automatic expression pipelining
    - Complete processor

