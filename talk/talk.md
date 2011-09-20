% Running a startup on Haskell
% Bryan O'Sullivan
% mailrank.com

# Steal these slides!

[github.com/bos/strange-loop-2011](https://github.com/bos/strange-loop-2011)


# In the beginning

I started with Haskell as an undergraduate in 1993

* Took a ten-year "time to earn a paycheque" break

* Worked lots in Java, C++, Python, C ... you name it

* Linux kernel hacking, supercomputers, distributed systems, set-top
  boxes, all kinds of fun projects


# My return to the fold

In about 2005, my interest in Haskell rekindled.

What did I see?

* Many of the same people I had known were still around.

* Much progress had been made.

* The community seemed to be on the cusp of ... something. But what?


# Relearning to walk

When I came back to Haskell, I once again found it daunting.

* There were no appropriate books available.

* Tutorial information was scattered all over.

* Too much was buried in academic papers.

"Someone should really write a book," I said.


# A bit of advice

When you hear someone say:

* "There ought to be a ..."

* "Someone should ..."

Ask a question:

* "So when are you going to get going on that?"


# Writing

I started planning a book in late 2006.

* Pitched O'Reilly. They bit.

* Found some collaborators.

* Started work in May 2007, finished 15 months later.


# Real World Haskell

[realworldhaskell.org](http://www.realworldhaskell.org/)

It's all available online, for free

Continues to sell well after 3 years - yay!


# Meanwhile

I'd worked in startups on and off for years

While I was working on Real World Haskell, I took a "startup
sabbatical" at Linden Lab (makers of Second Life)

Late last year, I was getting hungry to start something new

I started talking with some former colleagues

We found a compelling idea and seed funding, and got to work


# MailRank

My company has been in business almost a year

We build an awesome product for helping people deal with their email

It's a hybrid app

* The desktop piece is in C#

* Cloud components are in Haskell


# Scary decisions

Starting a company is a risky job.

You become acutely aware of this when you're making all the technical
decisions.

That sense of acuity quadruples when it's an investor's money you're
spending.


# Why Haskell?

Criteria that worked well for me:

* Do I know the language?  Can I be productive fast enough?

* Can I hire good people?

* Do I know the terrain well enough to be able to predict and navigate
  problems?


# Language knowledge

Not as big a roadblock as you might think.

* On the one hand, I was a Haskell expert.

* On the other, I never wrote a line of C# or used a modern IDE before
  December.
  
Both languages have worked out well, despite the difference in prior
experience.


# Hiring: signal

People who know Haskell well are of course thin on the ground.

The coding chops of Haskell experts are mind-blowing.

Even people who are *interested* in Haskell tend to have desirable
characteristics, e.g. intellectual curiosity, that can be predictive
of good hires.
  
(The same tends to be true of Clojure and Scala.)

There are plenty of *enthusiastic* almost-Haskellers out there, so if
you don't mind taking a risk, the pool is big.


# Hiring: noise

The more widespread a characteristic in a population, the less
powerful it is as a filtering signal.

* "Jane knows Haskell" does not imply "Jane is smart".

* But uncommon characteristics hint that "Jane might have some other
  qualities that I should take a closer look at".

Python used to be a good "knows language X" filtering characteristic,
but no longer.

The presence or absence of C# on a resume is completely useless to a
hiring manager at a startup.


# The daily Haskell grind

What does working with Haskell involve?

* Write a few lines of code.

* Load it into the interactive interpreter (`ghci`).

* Did the interpreter give a type error?  Fix it.

* Did the code load?  Try it out by hand.

* Did it work? Build the native-code app and try that.


# Differences from other environments

The "edit, then mess around in the interpreter" cycle feels a lot like
Python or Lisp.

* With the exception of the typechecker telling you how loopy your
  reasoning is.

The "compile a native app" part feels a lot like C++.

The "lack of IDE support" bit feels a bit like 1994.


# And now for an aside

Speaking of 1994 ...

Anyone remember that year? 

Remember Ace of @#%^$ Base?


# The cloudy bit

So all the Haskell is up in the cloud somewhere.

How's that work?

There's an SSL-secured HTTP-based server component.

It's vaguely RESTful.


# What that Haskell code does

It uses bulk anonymized data to do machine learning work.

The bulk data is managed in a data store.


# The data store

We started out experimentally, using the Riak distributed key-value
store.

Wrote our own bindings:

* [github.com/mailrank/riak-haskell-bindings](https://github.com/mailrank/riak-haskell-client)


# Our experience with Riak

Our bindings are sleek and solid, and Riak works well, but it had two
fatal shortcomings:

* At the time, no secondary indices for data

* Vector clocks are like having a smoke alarm but no fire extinguisher

So we dropped it.


# From Riak to ...

... MySQL.  Yes, really :-)

Everyone hates on MySQL, but remember what I said earlier?

* The more widespread a characteristic in a population, the less
  powerful it is as a filtering signal.

Everyone hates their data store, *no matter what it is*.

So you can't use the "hatred of data store" signal to make decisions.

MySQL is actually pretty decent, and I say that with years of
experience using it in high-volume production shops.  (And yes,
cursing it, too.)


# However ...

We're going to see a theme emerging here.

We wrote our own MySQL bindings:

* [github.com/mailrank/mysql-simple](https://github.com/mailrank/mysql-simple)

I wasn't happy with either the API or the performance provided by the
de facto standard Haskell database interface.

Our bindings are really easy to use, and far faster than their
predecessor.

Writing the bindings, and porting our app from Riak to MySQL, took
about 2 days.


# What about the HTTP bit?

After I was done with the Haskell book, one project I worked on was
improving network I/O performance in GHC.

You can see some results of this work in the Warp web server:

* 90,000+ requests per second

There are two good web frameworks available:

* Yesod (based on Warp)

* Snap (which I use)


# And data?

Happily, JSON has become the easiest and most widespread way for
clients and servers to talk.

And (surprise!) we wrote our own JSON library:

* [github.com/mailrank/aeson](https://github.com/mailrank/aeson)

Again, easier to use and much faster than its predecessor.


# Other bits

We wrote a fast, thread safe resource pooling library:

* [github.com/mailrank/pool](https://github.com/mailrank/pool)

A library for handling app configuration:

* [github.com/mailrank/configurator](https://github.com/mailrank/configurator)

And plenty more libraries besides.


# Building

We use the standard Haskell build tool, `cabal`.

Actually, we use a sandboxed version named `cabal-dev`, which gives
more reproducible builds.

Jenkins for continuous builds.

The units of deployment are tarballs containing executables,
configuration, and other data.


# Testing

Haskell has ridiculously good testing support.

* The QuickCheck library - I cannot sing its praises highly enough.

* *Very* different from traditional tests.

* Generate random data, then tell me if all properties that I expect
  to be true of my functions and data hold.

QuickCheck is *shockingly* more effective at finding bugs than unit
tests.


# Property based testing

If you walk out of this talk with nothing else in your head (and let's
be fair, it's really early in the morning), remember this:

* I must hunt down and learn to use a QuickCheck-like library for my
  language.
  
* If I can't find one, I should write one.


# So that means Haskell code is bug-free, right?

Let me tell you about the best compiler bug ever.


# Other moving parts

In addition to the server, we have some big-data number crunching apps
that we run over our data periodically.

Also written in Haskell.

The code is beautifully expressive.

It's also damn fast.


# Deployment

Deploying our code is a simple matter of redirecting a symlink, then
bouncing the server.

Downtime during a deploy is a fraction of a second.


# How has it worked out?

We're close to a public beta.

In our private beta, the app has been rock solid.

We've written our own HTTP load tester to beat it up.

(Think ApacheBench or `httperf`, only more modern, easier to use, and
with solid math behind its analysis.)

* [github.com/mailrank/pronk](https://github.com/mailrank/pronk)

(Not released yet either, but take it for a spin if you like.)


# Haskell compared with C#

So we've also been writing a ton of C#.  How has that gone?

* It's a decent language, but feels very complicated for what it does.

* LINQ is pretty nice (but see "complicated").

* Even for an Emacs/`vi` veteran, working in an IDE has its upsides.

* Reshaper is pretty sweet.


# Types

After a language with decent type inference, C#'s local inference
feels like "little Bobby's first steps".

Some language features (smart getters and setters) encourage really
shitty API design, and Microsoft itself goes to town with them.

* I'm looking at you, object property that sneakily gives back an
  entirely different COM handle every time it's dereferenced!


# Concurrency

After using STM (software transactional memory), concurrent
programming in C# makes me feel sad, old, and stupid.

And I'm only two of those things on good days.

* *By far* our single hardest-to-find bug was a concurrency problem in
  a huge, complicated app that we could only partly debug (tons of
  third party code).

The legacy hold-over of single-threaded COM apartments and other such
cruft can be very painful.


# So, um, yeah

C# is pretty decent.

Except for the times when it launches the chainsaw.

From the basket of mewling kittens.

Straight into your jugular.


# Should you be using Haskell?

Let's suppose you have the inclination to do so.

* For many purposes, you'll find good libraries and tools to help you,
  almost all with liberal BSD licensing.

* The teaching materials are good (three great, modern books on the
  market, two of them free online).

* The community is very helpful.

* You'll be able to find people to hire.

So it's a slam dunk, right?


# The basic decision criterion

You *must* give yourself time to stumble!

For all languages, tools, or technologies *X*:

* *if* you don't know *X*,

* *and* you don't have a sure source of time and money for the time
  it'll take to become moderately proficient,
  
* *and* you plan to use *X* anyway,

* *then* please see me after the talk, as I have a lead on this nice {
  bridge | weight loss plan | investment from Nigeria } that you might
  be interested in!


# Next steps

We're continuing to write new code.

Going to be hiring soon!

Teaching "Functional Systems in Haskell" at Stanford, starting next
week.

Thanks!
