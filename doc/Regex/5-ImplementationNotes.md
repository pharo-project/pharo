# Implementation notes

Version:			1.1
Released:		October 1999
Mail to:			Vassili Bykov <vassili@parcplace.com>, <v_bykov@yahoo.com>
Flames to:		/dev/null

## What To Look At First
- `String>>#matchesRegex:` in 90% of cases this method is all you need to access the package.
- `RxParser`: accepts a string or a stream of characters with a regular expression, and produces a syntax tree corresponding to the expression. The tree is made of instances of Rxs<whatever> classes.
- `RxMatcher`: accepts a syntax tree of a regular expression built by the parser and compiles it into a matcher: a structure made of instances of Rxm<whatever> classes. The `RxMatcher` instance can test whether a string or a positionable stream of characters matches the original regular expression, or search a string or a stream for substrings matching the expression. After a match is found, the matcher can report a specific string that matched the whole expression, or any parenthesized subexpression of it.
- All other classes support the above functionality and are used by `RxParser`, `RxMatcher`, or both.

## Caveats
The matcher is similar in spirit, but NOT in the design — let alone the code — to the original Henry Spencer's regular expression implementation in C. The focus is on simplicity, not on efficiency. I didn't optimize or profile anything. I may in the future — or I may not: I do this in my spare time and I don't promise anything. The matcher passes H. Spencer's test suite (see 'test suite' protocol), with quite a few extra tests added, so chances are good there are not too many bugs. But watch out anyway.

## Extensions, Future, etc.
With the existing separation between the parser, the syntax tree, and the matcher, it is easy to extend the system with other matchers based on other algorithms.
