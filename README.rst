hs
==

My goal with this repository, is to eventually become proficient in Haskell.
To do that, I'm going to start with a few very small, very simple projects and then work up to much more complicated ones.

Of course, with the bit of dephell that Haskell has, I am not sure how long I will be able to stick with it.

Additionally, since a friend of mine (besenwesen) introduced me to an awesome benchmarking tool called `criterion <http://www.serpentine.com/criterion/tutorial.html>`_, I've felt a strong urge to test it.
I would not be surprised if I start adding benchmarks to many portions of my code.

Below is my tentative plan for projects:

To Do
-----

- [X] Hello, World!
- [X] FizzBuzz
- [X] Happy Sequence / Happy tester
- [X] Collatz Conjecture tester
- [X] Add ``-s`` options to happy and collatz to print the sequence for a given input
- [ ] Purify/cleanup happy/collatz
- [-] Module to allow for representing integers in more obscure radicies
- [X] Basic status output program to pipe into dwm's panel

  - [ ] Eventually replace external process calls with modules (which I may have to write)
  - [X] Move configuration to command-line options

- [ ] Fix ``lsip``'s ``-4``, ``-6`` and ``-a`` options
- [ ] Fibonacci Generator (Fast fibs algorithm)
- [X] ``hr`` implementation
- [-] silo and cabalize each project so they are each simply buildable with ``cabal build``
- [X] Quine
- [ ] Basic ``echo`` or ``cat``
- [ ] vty-ui IRC client
- [ ] IRC Bot
- [ ] Bindings for `IUP <http://webserver2.tecgraf.puc-rio.br/iup/>`_ (?)
- [ ] Something with IUP bindings (?)
