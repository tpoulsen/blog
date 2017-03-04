---
title: Obfuscating FizzBuzz
tags: haskell, functional programming
---

<a href="https://en.wikipedia.org/wiki/Fizz_buzz">FizzBuzz</a> is an exercise based on a kid's game in which one counts from 1 to 100. The catch is that multiples of 3 are replaced with the word "Fizz", multiples of 5 with the word "Buzz" and multiples of both 3 and 5 with "FizzBuzz." Writing a program to perform this task should be easy, but annecdotally its use as a test during interviews <a href="http://blog.codinghorror.com/why-cant-programmers-program/">apparently</a> gives a large number of job applicants trouble.

Because the most basic solution is really easy to write, I spent a little time playing around with writing more complicated solutions. For fun. Because why not?

The basic solution:
```haskell
fizzBuzz :: Integer -> String
fizzBuzz x =
    |x `mod` 15 == 0 = "Fizzbuzz"
    |x `mod` 5  == 0 = "Buzz"
    |x `mod` 3  == 0 = "Fizz"
    |otherwise       = show x
```
A brief walkthrough of the Haskell syntax:
```haskell
fizzBuzz :: Integer -> String
```
The first line is the type declaration. fizzBuzz is the functions name. The double colon (::) means "has the type of," and Integer -> String means the function takes an Integer and returns<sup>1</sup> a String.

```haskell
fizzBuzz x =
    |x `mod` 15 == 0 = "FizzBuzz"
    |x `mod` 5  == 0 = "Buzz"
    |x `mod` 3  == 0 = "Fizz"
    |otherwise       = show x
```
The remaining lines are the function definition itself. fizzBuzz takes one argument (an Integer), referred to as "x" in the function. The (|) is guard syntax -- the guard expressions are checked from top to bottom and when one matches, the function evaluates to what is on the right of the equals sign. The guard expressions are basically booleans, and when they evaluate to True, the String that the function returns the appropriate string. "otherwise" is a catch-all and will always evaluate to True. "show x" is itself a function that returns a String representation of whatever x is, in this case a string literal of the Integer that was passed in.

Loading this function in GHCi (Haskell's REPL), it performs exactly as you would expect:
```bash
ghci> fizzBuzz 3
"Fizz"
ghci> fizzBuzz 5
"Buzz"
ghci> fizzBuzz 1
"1"
ghci> fizzBuzz 15
"FizzBuzz"
--Mapping the function over a list of numbers from 1 to 50 we get a list of the Strings returned:
ghci> map fizzBuzz [1..50]
["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","Fizzbuzz","16","17",
 "Fizz","19","Buzz","Fizz","22","23","Fizz","Buzz","26","Fizz","28","29","Fizzbuzz","31","32","Fizz",
  "34","Buzz","Fizz","37","38","Fizz","Buzz","41","Fizz","43","44","Fizzbuzz","46","47","Fizz","49","Buzz"]
```

Obfuscation 1
====

```haskell
fizzBuzz' :: Int -> String 
fizzBuzz' x = unwords $ take x [if x `mod` 15 == 0 then "FizzBuzz" else 
                                if x `mod` 3 == 0 then "Fizz" else 
                                if x `mod` 5 == 0 then "Buzz" else show x | x <- [1..]]

```

Instead of taking and checking a single Integer, this function takes an Int as its stopping point (<i>e.g.</i> it runs FizzBuzz from 1 to x). It uses a list comprehension, an infinite list, and a series of <i>if-then-else</i> statements to actually check the numbers in the range bounded by the Integer argument. Because Haskell is a lazy language, we're able to use infinite lists; numbers in the list are generated on an as-needed basis and because we use the function "take x" on the list comprehension, it only calculates the range up to x. This results in a list of Strings that the function *unwords* concatenates into a single String, as per the function's type signature.

Usage:
```bash
ghci> fizzBuzz' 50
"1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz Fizz 22 23 
Fizz Buzz 26 Fizz 28 29 FizzBuzz 31 32 Fizz 34 Buzz Fizz 37 38 Fizz Buzz 41 Fizz 43 44 
FizzBuzz 46 47 Fizz 49 Buzz"
```

Obfuscation 2
====

```haskell
import Control.Applicative ((<*>), (<$>))

fizzBuzz'' :: Integer -> String
fizzBuzz'' x = if fizzBuzz x then "FizzBuzz" else
               if fizz x then "Fizz" else
               if buzz x then "Buzz" else
               show x
            where fizz     = (==0) <$> flip mod 3    -- Integer -> Bool
                  buzz     = (==0) <$> flip mod 5    -- Integer -> Bool
                  fizzBuzz = (&&)  <$> fizz <*> buzz -- Integer -> Bool
```

There are some new concepts here. The first line is an import of the Control.Applicative module, specifically importing the functions (<\*>) and (<$>). Applicative functors are an awesome abstraction; they present calculations within a certain context (<i>e.g.</i> possible failure - Maybe, non-determinism - Lists, etc...) and apply them to values. <$> is a convenience function for fmap, so it maps a function over something in a context and <\*> applies a contextual value to the function<sup>2</sup>. 

The initial part of the function is similar to what we've seen before with its series of <i>if-then-else</i> statements. The *where* keyword allows us to define functions or names inside the scope of the parent function. Here, I'm using it to define a few helper functions that take Integers and return Booleans: *fizz*, *buzz*, and *fizzBuzz*. 

In the case of the first two, the partially applied function (==0) is mapped over the result of <i>flip mod 3</i> (or mod 5 depending on the function). *flip* is a function that takes another function as its argument and flips the order of the second functions arguments. Its use here on *mod* allows us to partially apply 3 or 5 to the function so that when *x* is supplied as an argument, we end up getting <i>mod x 3</i> or <i>mod x 5</i>. *flip*, by the way is an extremely handy function! When these functions are called with an Integer, the modulo operation occurs and then the result is applied to (==0), resulting in either a True or a False. *fizzBuzz* maps the logical 'AND', (&&) over the function fizz and <*> applies buzz, resulting in a single function that takes one integer and returns True if both *fizz* and *buzz* are true for that integer.

Usage:
```bash
# fizzBuzz'' takes a single Integer, so we're mapping it over the list [1..50] and 
#   concatenating the resulting list into a single String with unwords. 
#   Using the (.) composes the two functions.
ghci> unwords . map fizzBuzz'' $ [1..50]
"1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz Fizz 22 23 Fizz 
Buzz 26 Fizz 28 29 FizzBuzz 31 32 Fizz 34 Buzz Fizz 37 38 Fizz Buzz 41 Fizz 43 44 FizzBuzz 
46 47 Fizz 49 Buzz"
```

Obfuscation 3
====

```haskell
import Control.Applicative ((<*>), (<$>))
import Control.Monad

fizzbuzz''' :: Integer -> String
fizzbuzz''' x = if divisible /= "" then divisible else show x 
    where divisible = (guard (fizz x) >> "Fizz") <|> (guard (buzz x) >> "Buzz")
          fizz = (==0) <$> flip mod 3 
          buzz = (==0) <$> flip mod 5
```
The most consise of the lot, there are again some familiar constructs here (<i>if-then-else</i>, *where*, *flip*, and <i><$></i>) along with a couple new ones. Using Strings as monads<sup>3</sup>, applicative functors, and monoids<sup>4</sup> makes handling the FizzBuzz tests a lot more fun than in the first examples. 

Here, the *guard* function takes a Boolean (the result of fizz or buzz being applied to x) and if True, it performs some monadic action. <i>(>>)</i> is a monadic operator that throws away the result of the previous function and returns the value to its right (here either Fizz or Buzz depending on where in the function one is). If the Boolean is False, it (in this case) returns an empty String. <i><|></i> is an associative binary operator -- it takes two arguments (here, Strings) and applies the relevent associative operation (++, or concatenation).

So, what divisible does, is as follows:
<ol>
<li>If <i>fizz x</i> is True -> "Fizz", else -> ""</li>
<li>If <i>buzz x</i> is True -> "Buzz", else -> ""</li>
<li>The results of the 1 and 2 are concatenated together.</li>
</ol> 

*divisible* can result in "Fizz", "Buzz", "FizzBuzz", or "". In the <i>if-then-else</i> statement, if divisible is not an empty String, then divisible is the output of the function, else a String representation of the number is the output.

Usage:
```bash
ghci> unwords . map fizzbuzz''' $ [1..50]
"1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz 
Fizz 22 23 Fizz Buzz 26 Fizz 28 29 FizzBuzz 31 32 Fizz 34 Buzz Fizz 37 38 Fizz 
Buzz 41 Fizz 43 44 FizzBuzz 46 47 Fizz 49 Buzz"
```

------
<sup>1</sup>While "return" has a meaning in Haskell that is not the equivalent of return in languages like C and Python, in this context I'm using it in the traditional way.

<sup>2</sup>This is a little hand-wavey, but going into a full explanation of Functors and Applicative Functors is beyond the scope of this post.

<sup>3</sup>Another powerful construct that gives code structure or context. Much ink has been spilled on attempts at explaining monads, but the best way to get a handle on them is to use them. 

<sup>4</sup>Monoids are a category that have an identity function and a binary operation. Common examples include addition (identify = 0, binary operation = +) and multiplication (identity = 1, binary operation = *).
