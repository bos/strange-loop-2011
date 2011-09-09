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
