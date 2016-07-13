# london-calling: detect dodgy Git practices

## Erm, what?
Detects commits directly to `master` on a single local Git codebase. 
More specifically, it finds and outputs tab-separated summaries of all the commits on `HEAD` not merged from another branch.

## How?

    $ london-calling --exclude bad@robot.org ~/workspace/quodlibet
    ...
    6020694	2016-07-10	Ratings menu: only show checkmarks for songs with ratings	(naughty@developer)
    2f6b13f	2016-07-12	Add an MQTT publishing event plugin.	(naughty@developer)
    62d83b0	2016-07-12	Remove unused imports	(naughty@developer)

## Why is this useful?
Well, _let me tell you a story_&hellip;
Once upon a time there was a start-up.
Developers loved their BDD, TDD, pairing and peer review... Maybe a bit too much even.
They used GitHub PRs from branches because they're ~~less hassle~~ more generic than forks. 
Anyway, then more developers joined and deadlines started coming as fast as the pivots were happening.

And actually not all of them _did_ love having their code reviewed, 
maybe because they were _Ninja Rock-Star 10x_ devs, or something. 
But more and more would pull `master` and find it had magically changed despite no PRs being merged.
Rebasing became more fun. Tickets didn't get auto-updated. CI pushed things straight to environments before reviews...

## Does it actually work?

Yes, to an extent. It's very _alpha_ (erm, as in _version_, not... _male_) right now but a few real-world tests
show reasonable results. It's not super-efficient with large codebases, mind you,
so you may need to wait a few seconds (or more...)


## What's this thing written in then?
[Haskell](https://haskell-lang.org/), because it's equal parts wonderful and brain-meltingly infuriating (for newbies like myself at least)

It uses the [libgit wrapper](https://hackage.haskell.org/package/libgit) for higher-order Git querying,
and the excellent [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) library 
to make the (simple) CLI applicative, typesafe, and generally not get in the way.

## FAQ
### Seriously what's up with that name?
Made sense at the time / had to be there, etc.
Someone coined the term and then it stuck (but arguably: validated the laziness ongoing...)

### But our workflow is better

Probably is. Notably - public Github projects (with forks) aren't really what this is for 
(though that would be interesting) - it's for _single_ shared repos probably used by small / young companies.

### But can't you just use the Git CLI?

Well, err, yes, with a bit of magic, it turns out you can. D'oh:

```shell
git log --format="%h %s (%ae)" --no-merges --first-parent master | grep -v bot@domain.com
```

But then, only [smug bastards would know that](http://think-like-a-git.net/) I guess.

### How about supporting remote github repos?
Yes, that might be interesting, though cloning is fast and pretty easy so of somewhat limited value.
One of the reasons for choosing the [hs-libgit](vincenthz/hs-libgit) library was its support for multiple back-ends.
