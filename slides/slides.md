% Haskell: Functional Programming, Solid Code, Big Data
% Bryan O'Sullivan
% 2011-09-18

# Welcome!

~~~~ {.haskell}
main = putStrLn "hello!"
~~~~

* My name is Bryan O'Sullivan

* I started using Haskell in 1993

* I wrote a book about it, "Real World Haskell"

    * [realworldhaskell.org](http://book.realworldhaskell.org/)

* I write lots of open source code

    * [github.com/bos](https://github.com/bos)
    
* My company invests heavily and openly in Haskell

    * [github.com/mailrank](https://github.com/mailrank)


# My Haskell background

* Began using Haskell at university

* When I graduated in 1995, I switched to more mainstream languages

    * C, C++, Java, Python, Perl, etc.

* My interest reawakened around 2005

* I've found Haskell to be a great general-purpose language

* The community of people is amazing


# What to expect

* This is a *hands-on* workshop: you'll be writing code!

* There will be a short break every 45 minutes

* Don't be afraid to ask questions!


# Your tools

* You've already installed the Haskell Platform, right?

    * [hackage.haskell.org/platform](http://hackage.haskell.org/platform/)

* This gives us a great toolchain

    * The GHC compiler (`ghc`)

    * The GHCi interpreter (`ghci`)
    
    * The Cabal package manager (`cabal`)
    
    * Some handy libraries and tools


# What else is needed?

* A text editor

* A terminal window


# Let's get started!

Create a file named `Hello.hs` and give it the following contents:

~~~~ {.haskell}
main = putStrLn "hello, world!"
~~~~

The suffix `.hs` is the standard for Haskell source files.

File names start with a capital letter, and everyone uses `CamelCase`.


# Building it

This command will look for `Hello.hs` in the current directory, and
compile it:

~~~~
ghc --make Hello
~~~~

The generated executable will be named `Hello` (`Hello.exe` on
Windows).

* That `--make` option up there tells GHC to automatically deal with
  dependencies on source files and packages.


# Checking in

Is everyone able to build and run their `Hello` executable?


# Something a little more convenient

It's nice to have fast, native code at our fingertips.

But when *I'm* working, I expect a few things:

* I do lots of exploration.

* I make tons of mistakes.

For these circumstances, a full compiler is a bit slow.

Instead, I often use the interactive interpreter, `ghci`.


# Let's start GHCi

Easily done:

~~~~
ghci
~~~~

It will display a startup banner, followed by a prompt:

~~~~
Prelude>
~~~~

This default prompt tells us which modules are available to play with.


# Play around

The `ghci` interpreter evaluates expressions interactively.

Try it out:

~~~~ {.haskell}
2 + 2
~~~~

~~~~ {.haskell}
"foo" ++ "bar"
~~~~

(That `++` is the string append operator.)


# Directives

All interpreter directives start with a "`:`" character.

Let's load our source file into `ghci`:

~~~~
:load Hello.hs
~~~~

Now the `ghci` prompt changes:

~~~~
*Main>
~~~~


# Running our code in ghci

We defined a function named `main`, so let's invoke it:

~~~~
main
~~~~

Did that work for you?


# A few more useful directives

Remember, they all start with a "`:`".

* `:help` tells us what directives are available.

* `:reload` reloads the file we last `:load`ed.

* `:edit` launches our text editor on the file you most recently
  `:load`ed.  (Does *not* automatically `:reload`.)
  
* `:quit` exits back to the shell.


# Final ghci efficiency tips

We can abbreviate directives:

* `:e` is treated as `:edit`

* `:r` is `:reload`

We also have command line history and editing.

* On Unix, compatible with `readline`.

* On Windows, the same as `cmd.exe`.


# Getting used to the cycle

Use `:edit` or your text editor to change the "hello" text.

Use `:reload` to reload the source file.

Test out your redefinition of `main`.

* For practice, hit the "up arrow" key to cycle through your command
  history until you get back to the last time you typed `main`.


# Problem definition

Given a web site, we want to scrape it and find important web pages.

This involves a lot of figuring stuff out!

* Download one web page

* Extract links from a page, so we can find more pages to download

* Once we're done, compute which ones are important

* Make it all fast?


# Let's download a web page!

We'd really like to rely on a library to download a web page for
us.

For stuff like this, there's a very handy central repository of open
source Haskell software:

* [http://hackage.haskell.org](http://hackage.haskell.org/)

* (Everyone just calls it "Hackage")

Go there now!

Click on the
[Packages](http://hackage.haskell.org/packages/archive/pkg-list.html)
link at the top of the page to browse packages.

Alas, the list is overwhelmingly long, but we can find libraries for
all kinds of tasks if we're patient.

Are we patient?


# Ugh!

We don't want to look through thousands of libraries - surely there's
a better way?

Enter the `cabal` command!

Run this command in a terminal window:

~~~~
cabal update
~~~~

This downloads the latest index of all software on Hackage.

With the index updated, we can search it:

~~~~
cabal list http
~~~~

That still gives us 20+ packages to comb through, but at least it's
better than the 3,400 on the Packages web page.


# Short-cutting the search

The best HTTP client library is named `http-enumerator`.

We can read about it online:

* [http://hackage.haskell.org/package/http-enumerator](http://hackage.haskell.org/package/http-enumerator)

That landing page for a package is intimidating, but look towards the
bottom, at the section labeled "Modules".

What do you see?


# Installing a package

To install the `http-enumerator` package, we just issue a single
command:

~~~~
cabal install http-enumerator
~~~~

This command figures out all the other libraries that
`http-enumerator` depends on, and downloads, compiles, and installs
the whole lot.

Expect it to take a few minutes and print a lot of output.


# Reading docs: packages and modules

While we're waiting for the `http-enumerator` package and all of its
dependencies to install, let's try to figure out how we should use it.

Remember the link to API documentation at the end of the package's web
page? Click through to the API docs.

An API page begins with a title that looks something like this:

~~~~
Network.HTTP.Enumerator
~~~~

This is the name of a *module*.

A module is a collection of code.

A *package* is a collection of modules.

(This will sound familiar if you know Python.)


# Reading docs: the rest

After the initial blurb, a module's docs consist of type signatures and
descriptions.

Here is a really simple type signature:

~~~~
foo :: String
~~~~

How the heck do we read this?

The *name* of the thing being defined comes before the `::`
characters.

Its *type* follows after the `::`.

This means "the value named `foo` has the type `String`".


# A function signature

Here's another type signature:

~~~~
words :: String -> [String]
~~~~

Here we see a new symbol, `->`, which means "this is a function".

The type after the last `->` is the return type of the function.

All of its predecessors are argument types.

So this is a function that takes one `String` argument, and which
returns ... what?


# List notation

The notation `[a]` means "a list of values, all of some type `a`".

So `[String]` means "a list of values, all of type `String`".


# Words

~~~~
words :: String -> [String]
~~~~

We can now read that this function accepts a string as argument, and
returns a list of strings.

From reading its name and type signature, can you guess what `words`
might do?


# Another signature

Tell me about this signature:

~~~~
unwords :: [String] -> String
~~~~


# Reading real-world docs

Here is the very first signature from `http-enumerator`:

~~~~
simpleHttp 
  :: (MonadIO m, Failure HttpException m) => 
     String -> m ByteString
~~~~

This is more complex! How the heck do we read it?

The bits between `::` and '=>' are *constraints* on where we can use
`simpleHttp` - but let's ignore constraints for now.

We'll also ignore that mysterious lowercase `m` for a bit.

What can we tell about this function?


# Let's play in ghci!

Does everyone have `http-enumerator` installed now?

Fire up `ghci`, and let's play with the module:

~~~~
import Network.HTTP.Enumerator
~~~~

Notice that after we type this, the prompt changes:

~~~~
Prelude Network.HTTP.Enumerator>
~~~~

This tells us that the module has loaded and is available.


# Wait! Are you on Windows?

On Windows, we have to set up Winsock before any networking will work.

First, let's load the lowest-level networking module:

~~~~
import Network.Socket
~~~~

And here's how we initialize Winsock:

~~~~
withSocketsDo (return ())
~~~~


# With that out of the way ...

Finally - let's load a web page!

~~~~
simpleHttp "http://example.com/"
~~~~

Did that just print a ton of HTML in the terminal window?  All right!
