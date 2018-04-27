# graphql-row-types

this is an experiment with using row types to describe a graphql schema which
allows for cyclical dependencies by the use of a fixed point and symbol references

use `stack build`, there is currently no meaningful executable to run

at the moment the type family solving is prohibitively slow, switching to something like
https://hackage.haskell.org/package/type-level-bst may help, or using a typechecker plugin
like coxswain
