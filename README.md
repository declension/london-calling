# london-calling: detect dodgy Git practices

## Erm, what?
Detects commits directly to `master` on a single local Git codebase. 
More specifically, it finds and outputs tab-separated summaries of all the commits on `HEAD`
that **don't** meet **any** of these criteria:

 * merged from another branch
 * in the excluded email list (e.g. commit bots)
 * committed by someone other than the author (as per squashed branch merges).

## How?

```bash
$ london-calling --exclude bad@robot.org ~/workspace/quodlibet
...
6020694	2016-07-10	Ratings menu: only show checkmarks for songs with ratings	(me@example.com)
2f6b13f	2016-07-12	Add an MQTT publishing event plugin.	(me@example.com)
62d83b0	2016-07-12	Remove unused imports	(me@example.com)
```

## But how do I build it?
Assuming you have [Stack](http://docs.haskellstack.org/en/stable/README/) set up (and your `~/.local/bin` in your path),
you just need to:

```shell
stack build && stack install
```

That was easy.

## Why is this useful?
Well, _let me tell you a story_&hellip;

Once upon a time there was a start-up (in London).
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
so you may need to wait a few seconds (or even minutes...)


## What's this thing written in then?
[Haskell](https://haskell-lang.org/), because it's equal parts wonderful and brain-meltingly infuriating (for newbies like myself at least)
It's built in the game-changing [Stack](http://docs.haskellstack.org/en/stable/README/) because it's made Haskell ~~bearable~~ **fun** to build now,
Being a small, simple project it uses precisely _none_ of the latest GHC features it is pegged against (but _never look back_ etc).

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

Well, err, yes, with a bit of magic, a _similar_ thing can be done:

```shell
git log --format="%h %cd %s (%ae)" --date=short --no-merges --first-parent | grep -v bot@domain.com
```

But then, only [smug bastards would know that](http://think-like-a-git.net/) I guess.

AIUI, this is only checking that the first parent of merges is on the branch, and will report more things than 
`london-calling` as it doesn't actually check it can find that commit from the other parent (branch) history,
nor does it care about authors vs committers. The native Git version is of course a _lot_ faster.

### How about supporting remote Github repos?
Yes, that might be interesting, though cloning is fast and pretty easy so of somewhat limited value.
One of the reasons for choosing the [hs-libgit](vincenthz/hs-libgit) library was its support for multiple back-ends.
