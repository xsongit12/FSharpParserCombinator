# FSharpParserCombinator
A simple parser combinator library written in F Sharp. Shamelessly based off of "Understanding Parser Combinators" from F# 
For Fun and Profit. Personal Project to understand functional languages and basic parsing.

## Installation

I mean, I guess you can just clone the repo and load the file "CombinatorialParser.fs" in F Sharp.
This exposes the library for use.

## JSON Parser

A simple JSON parser was written using the parser combinator library as a simple proof of concept that it works. And It does work, just maybe not perfectly. I haven't tested it extensively, just kinda threw a simple one at it and was happy that the whole thing parsed. 

## Why Would You Do Something that's already been done?

Well, stranger who is reading this, I am fully well aware that one can just import Parsec, FParsec, or whatever parser combinator library flavor you prefer and that renders this entire library moot (as it should). However, this ended up being the result of my summer efforts to understand functional programming and more specifically Monads. It turns out that really, a parser (or really a function) can be thought of as a Monad. 

In that sense, this project was a good education in functional programming in general.
