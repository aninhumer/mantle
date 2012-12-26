README
======

Mantle is a Hardware Description Language created as an embedded language in Haskell. It uses a monadic construct to build up a Register Transfer Level representation of hardware, which can then be compiled to synthesisable Verilog code. This can then be built upon using regular Haskell to provide complex and powerful abstractions for creating hardware designs.

Here is a simple counter example in Mantle, which uses an abstraction for Synchronous circuit designs:
```Haskell
counter :: SyncComp (Output Int)
counter out = do
    count <- reg 0          -- Create a register which is set to 0 on reset.
    onSync $ do             -- Then every clock edge,
        count <=: count + 1 -- increment the counter.
    out =: count            -- Set the output to match the current count.
```

I am developing Mantle as a third year undergraduate project at Cambridge University.