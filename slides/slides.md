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


# What to expect 1

* Haskell is a fairly big language

* Since so much is unfamiliar to newcomers, expect to wander far from
  your comfort zone

* I'm going to teach you *interesting* things, but not *everything*

# What to expect 2

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
123456781234567812345678 * 87654321876543
~~~~

~~~~ {.haskell}
"foo" ++ "bar"
~~~~

(That `++` is the "append" operator.)


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

What about this?

~~~~ {.haskell}
putStrLn "hi mom!"
~~~~


# A few more useful directives

Remember, all `ghci` directives start with a "`:`".

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


# Lists and strings

~~~~ {.haskell}
[1,2,3,4]
~~~~

~~~~ {.haskell}
['h','e','l','l','o']
~~~~

Double quotes are just syntactic sugar for the longer form:

~~~~ {.haskell}
"hello"
~~~~

What does this print?

~~~~ {.haskell}
"foo" == ['f','o','o']
~~~~


# Calling functions: 1

We use white space to separate a function from its argument:

~~~~ {.haskell}
head "foo"
~~~~

~~~~ {.haskell}
head [1,2,3]
~~~~

~~~~ {.haskell}
tail [1,2,3]
~~~~


# Calling functions: 2

If a function takes multiple arguments, we separate them with white
space:

~~~~ {.haskell}
min 3 4
~~~~

If an argument is a compound expression, wrap it in parentheses:

~~~~ {.haskell}
compare (3+5) (2+7)
~~~~

~~~~ {.haskell}
max (min 3 4) 5
~~~~


# Quick exercises: 1

Use `ghci` as a calculator.

The `**` operator performs exponentiation.

* If I invest 5 quatloos at 3% compound interest per annum, how many
  quatloos will I have after 10 years?


# Quick exercises: 2

The notation `['a'..'z']` generates a list from start to end,
inclusive.

The `sum` function adds the elements of a list.

* What is the sum of the numbers between 9 and 250, inclusive, *minus* 2?


# Quick exercises: 3

The `show` function renders a value as a string.  Try it!

~~~~ {.haskell}
show (1 == 2)
~~~~

The `length` function tells us how many elements are in a list.

~~~~ {.haskell}
length [1,2,3]
~~~~

* How many digits are in the product of all numbers between 0xBE and
  0xEF, inclusive?


# Defining a function

It is pretty simple to define a new function.

Open up your text editor, create a new file with a `.hs` extension,
and get writing!

~~~~ {.haskell}
isOdd x  =  (rem x 2) == 1
~~~~

* We start with the name of the function.

* Next come the names we want to give its parameter(s), separated by
  white space.

* After those come a single `=` character, with the *body* of the
  function following.

Load your source file into `ghci` and give `myOdd` a try.


# Making life more interesting

Now we can define very simple functions, but we're missing some
important building blocks for more fun.

So let's get to it!


# Conditional execution

Q: What does the familiar `if` look like in Haskell?

A: Familiar!

~~~~ {.haskell}
gcd a b = if b == 0
          then a
	  else gcd b (rem a b)
~~~~

We have the following elements:

* A Boolean expression

* `then` an expression that will be the result if the Boolean is
  `True`
  
* `else` an expression that will be the result if the Boolean is
  `False`


# Finally! A tiny bit about types

The two possible results of an `if` expression must have the same
type.

If `then` evaluates to a `String`, well `else` must too!

For instance, this makes no sense:

~~~~ {.haskell}
if True
then 3.14
else "wombat"
~~~~

We are forbidden from writing ill-typed expressions like this.


# What about else?

In imperative languages, we can usually leave out the `else` clause
after an `if`.

Not so in Haskell.

Why does this make sense for imperative languages, but not Haskell?


# A nearly trivial exercise

Write a function that appends `", world"` to its argument if the
argument is `"hello"`, or just returns its argument unmodified
otherwise.

* Remember, the "append" function is an operator named `++`.


# Lists in Haskell

We already know what a list looks like in Haskell:

~~~~ {.haskell}
[1,2,3]
~~~~

And of course there's the syntactic sugar for strings:

~~~~ {.haskell}
"foo" == ['f','o','o']
~~~~

But is this everything there is to know?


# List constructors

Supposing we want to construct a list from first principles.

* We write the *empty list* as `[]`.

* Given an existing list, we can add another element to the *front* of
  the list using the `:` operator.


# Type this into ghci

Add an element to an empty list:

~~~~ {.haskell}
1 : []
~~~~


# From single-element lists onwards

What about extending that list?

~~~~ {.haskell}
2 : (1 : [])
~~~~

You're probably guessing now that `[2,1]` is syntactic sugar for
`2:(1:[])`. And you're right!

What is the result of this expression?

~~~~ {.haskell}
5 : 8 : [] == [5,8]
~~~~


# Constructors

We refer to `[]` and `:` as *constructors*, because we use them to
construct lists.

When you create a list, the Haskell runtime has to remember which
constructors you used, and where.

So the value `[5,8]` is represented as:

* A `:` constructor, with `5` as its first parameter, and as its
  second ...

* Another `:` constructor, this time with `8` as its first parameter,
  and now as its second ...
  
* A `[]` constructor


# What did we see?

Depending on your background, I bet you're thinking something like
this:

* "Hey! Haskell lists look like singly linked lists!"

* "Hey! That looks just like lists built out of `cons` cells in Lisp!"

Right on.


# Why do we care about constructors?

So of course Haskell has to remember what a list is constructed of,
but it also lets *us* inspect a list, to see which constructors were
used.  How do we do this?

~~~~ {.haskell}
import Data.Char

isCapitalized name
  = case name of
      (first:rest) -> isUpper first
      []           -> False
~~~~


# Welcome to the case expression

A `case` expression allows us to *inspect* a structure to see how it
was constructed.

~~~~ {.haskell}
isCapitalized name
  = case name of
      []           -> False
      (first:rest) -> isUpper first
~~~~

* In between `case` and `of` is the expression we are inspecting.

* If the constructor used was the empty-list constructor `[]`, then
  clearly the `name` is not capitalized.

If the constructor used was the "add to the front" `:` operator,
then things get more interesting.
  
* Whatever was the first parameter of the `:` constructor is bound
  to the name `first`.
    
* The second parameter of the `:` constructor (i.e. everything in the
  list after the first element) is bound to the name `rest`.

* The expression following the `->` is evaluated with these values.


# Pattern matching

The `case` expression performs what we call *pattern matching*.

* Patterns are checked from top to bottom.

* As soon as a match is found, its right hand side (after the `->`) is
  used as the result of the entire `case` expression.
  
* If no match succeeds, an exception is thrown.


# A worked example

Let's step through the machinery of what happens if we evaluate this
expression.

~~~~ {.haskell}
isCapitalized "Ann"
~~~~


# Whew! A few exercises!

Finally! We can write slightly more complex functions.

Now that you can inspect the front of a list, you should be able to
process an *entire* list recursively.

First, please write a function named `myLength` that computes the
number of elements in a list.

Next, write a function named `countCaps` that calculates the number of
capital letters in a string.

~~~~ {.haskell}
countCaps "Monkey Butter" == 2
~~~~


# Counting capital letters

Wow, that countCaps function was a pain, right?

Here's my definition that uses only the machinery we've learned so
far:

~~~~ {.haskell}
countCaps string =
  case string of
    []     -> 0
    (x:xs) -> if isUpper x
              then 1 + countCaps xs
              else countCaps xs
~~~~


# Huh.

I thought Haskell was all about concision!?


# Conciseness 1: top-level pattern matching

~~~~ {.haskell}
countCaps []     = 0
countCaps (x:xs) =
    if isUpper x
    then 1 + countCaps xs
    else countCaps xs
~~~~

We can define a function as a series of equations, each containing a
pattern match.

This is nice syntactic sugar for `case`.


# Conciseness 2: guards

~~~~ {.haskell}
countCaps []     = 0
countCaps (x:xs)
   | isUpper x    = 1 + countCaps xs
   | otherwise    = countCaps xs
~~~~

After each `|` is a *guard*.

* If a pattern matches, we evaluate each Boolean guard expression from
  top to bottom.

* When one succeeds, we evaluate the RHS as the body of the function.

(Yes, patterns in a `case` can have guards too.)


# Before

Like the original version, but with use of `case` stripped out:

~~~~ {.haskell}
countCaps xs =
  if null xs
  then 0 
  else if isUpper (head xs)
       then 1 + countCaps (tail xs)
       else countCaps (tail xs)
~~~~

# After

Both shorter and easier to follow:

~~~~ {.haskell}
countCaps []     = 0
countCaps (x:xs)
   | isUpper x    = 1 + countCaps xs
   | otherwise    = countCaps xs
~~~~


# Another approach

Write a new version of `countCaps` as follows:

* Write a function that goes through a list, and which generates a new
  list that contains only its capital letters.

* Use `length` to count the number of elements.

This should give the same result as your first function. Right?


# A change of specification

Suppose we want to count the number of lowercase letters in a string.

This seems almost the same as our function for counting uppercase
letters.

What can we do with this observation?


# Higher order functions

*Higher order function*: a function that accepts another function as a
parameter.

~~~~ {.haskell}
filter pred [] = []
filter pred (x:xs)
  | pred x     = x : filter pred xs
  | otherwise  =     filter pred xs
~~~~

How can we use this to define `countLowerCase`?


# Data in, data out

By now, we've seen several definitions like this:

~~~~ {.haskell}
countLowerCase string =
  length (filter isLower string)
~~~~

This is a recurring pattern:

* A function of one argument

* It's being fed the result of ...

* ... another function of one argument


# Function composition

Haskell doesn't limit us to giving functions alphanumeric names.

Here, we define a function named simply "`.`", which we can use as an
operator:

~~~~ {.haskell}
(f . g) x = f (g x)
~~~~

How to use this?

~~~~ {.haskell}
countLowerCase = length . filter isLower
~~~~

# Understanding composition

If that seemed hard to follow, let's make it clearer.

We'll plug the arguments into the RHS of our function definition:

~~~~ {.haskell}
(f . g) x = f (g x)
~~~~

We had `length` as the first argument to "`.`", and `filter isLower`
as the second:

~~~~ {.haskell}
(length . filter isLower) x 
  = length (filter isLower x)
~~~~


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
