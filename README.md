Orchardia is a library in progress trying to improve editor tooling using clojure-clr, it is being made with the [Arcadia](https://github.com/arcadia-unity/Arcadia) environment in mind and we welcome requests to be used at other .NET projects and to improve the code itself.

This repository contains code stolen with some modifications where needed from [cider-nrepl](https://github.com/clojure-emacs/cider-nrepl), [compliment](https://github.com/alexander-yakushev/compliment) and [orchard](https://github.com/clojure-emacs/orchard) and [Arcadia](https://github.com/arcadia-unity/Arcadia), a big thank you for all the managers and contributors of these libraries!

## Goals

* Not to be feature complete with compliment, cider-nrepl or orchard.
* To be useful at the Arcadia environment (atm in conjunction with [miracle](https://github.com/Saikyun/miracle) for Emacs).

## Status

### Autocompletion (using `complement` and `arcadia`)
  * Keywords
  * Static members
  * Namespaces
  * Special forms

### Eldoc (using `cider.nrepl` and `orchard`)
  * Static methods
  * Clojure functions

## Wishlist
  * Build and Push this library to NuGet or some other .NET depedency manager (need help here!)
  * Add tests!
