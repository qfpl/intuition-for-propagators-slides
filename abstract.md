# An Intuition for Propagators

The propagator model of computation, developed by Radul and Sussman,
consists of stateful cells connected by independent stateless machines
called propagators. These propagator networks are a helpful way to
structure or think about computations, particularly those that are
concurrent or distributed.

This talk will give an intuition for what these propagator networks
look like, why they work, and how we can use them to build programs.
Efficient implementation will not be covered.

The Haskell community has recently seen concurrency abstractions
bearing similarities to propagators, such as the work on LVars by
Kuper and Newton; and also work directly influenced by propagators,
such as some of Edward Kmett's projects.

