Hoids
=====

This is a Haskell implementation of [Boids](https://en.wikipedia.org/wiki/Boids)

Development
===========
You can use what ever editor you like. Currently Eclipse and Vim have been used. 

Using Eclipse these use following steps to set up you environment.
 1. Install latest [Eclipse](http://www.eclipse.org/downloads/)
 2. Install [eclipsefp](http://eclipsefp.github.com/install.html)

Cabal is used to package the application so don't forget to add to Hoids.cabal

Using Emacs:
 ...

Building
========

You can fairly easy build it using:
```
ghc -package GLUT src/Main.hs -o Hoids
```

To package it we use cabal


