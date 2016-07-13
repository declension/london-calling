# london-calling: detect dodgy Git practices

## Erm, what?
Detects commits directly to `master` on a local Git codebase. 
More specifically, it finds and pretty-prints output for all the commits on `HEAD` not merged from another branch.

## Why is this useful?
Well, let me tell you a story.
Once upon a time there was a start-up. Developers loved their BDD, TDD, pairing and peer review using GitHub PRs.
Then more developers joined and deadlines started coming as fast as the pivots were happening.

And actually not all of them _did_ love having their code reviewed, 
maybe because they were _Ninja Rock Star 10x_ devs, or something. More and more others would pull `master` and 
find it had magically changed despite no PRs being merged. Rebasing became more fun. Tickets didn't get auto-closed.
CI pushed things straight to environments...


## What's this thing written in then?
[Haskell](https://haskell-lang.org/), because it's equal parts wonderful and brain-meltingly infuriating (for beginners like myself at least)

It uses the [libgit wrapper](https://hackage.haskell.org/package/libgit) for higher-order Git querying,
and the excellent [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) library 
to make the (very simple) CLI applicative, typesafe, and generally not get in the way.

##FAQ
### Seriously what's up with that name?
Made sense at the time / had to be there, etc.
Someone coined the term and then it stuck (but arguably: validated laziness ongoing...)


### But can't you just use the Git CLI?

Well, err, yes. Turns out you can.
```shell
git log --no-merges --first-parent master
```
### How about supporting remote github repos?
Yes, that might be interesting, though cloning is fast and pretty easy so of somewhat limited value.
One of the reasons for choosing the [hs-libgit](vincenthz/hs-libgit) library was its support for multiple back-ends.
