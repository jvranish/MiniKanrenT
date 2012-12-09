MiniKanrenT
===========

An implementation of miniKanren in Haskell

### To get started
clone the repository and open examples.hs like so:

>  ghci -isrc examples.hs
  
  
### Origins


This year at [strangeloop](https://thestrangeloop.com/)  I was totally blown away by the miniKanren talk by Daniel Friedman and William Byrd

I _highly_ recommend watching [the presentation](http://www.infoq.com/presentations/miniKanren). It is simultaneously entertaining and mind blowing.

They used an extended version of miniKanren, a logic programming language (a la prolog) embedded in scheme, to do several remarkable things:
 - Generate programs that evaluate to a given value
 - Generate programs that satisfy a given type
 - And most awesomely, generate [Quines!](http://en.wikipedia.org/wiki/Quine_(computing)) (and Twines!!)

This was something that I clearly did not know enough about yet.
Usually when I want to really learn and understand something in CS I try to implement it.

So I grabbed a copy of [The Reasoned Schemer](http://www.amazon.com/Reasoned-Schemer-Daniel-P-Friedman/dp/0262562146) and did just that!

My weapon of choice is Haskell, so here you go, a MiniKanren monad transformer in Haskell.

It turns out there is a very nice [monad transformer](http://hackage.haskell.org/package/logict) by Dan Doel based on a [paper](http://okmij.org/ftp/papers/LogicT.pdf) (co-authored by Friedman) that does the backtracking and interleaving for me.
I'll still likely have to take a shot at making my own version of logict (again, just to learn how it works) but this will work for now.

However, I couldn't find a suitable Haskell logic variable implementation. So I made one that can be safely used in a monad transformer. That means it will work correctly even with the backtracking and branching that the logic monad does. And it also means that I can basically plug the two monads together to make a miniKanren monad!


This is not quite the MiniKanren in the talk. They have 'does not unify' operator =/=  that I don't have implemented... yet...
But! I think it _does_ correctly implement the core features of miniKanren as described in The Reasoned Schemer.


I'm not quite satisfied with implementation yet. I still want to implement the remaining features necessary to be able to generate my own quines :)
I also want to implement some of the "libraries" for working with numbers and lists and such. 

So there should be more to come :)
