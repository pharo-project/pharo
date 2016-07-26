I represent Strings that are created uniquely. Thus, someString asSymbol == someString asSymbol.

To see the difference between Symbol and Strings look at this example:

| s1 s2 |

s1 := 1234 asString.
s2 := 1234 asString.

"Strings are not unique, while Symbols are"

s1 = s2. "true"
s1 == s2. "false"

s1 asSymbol = s2 asSymbol. "true"
s1 asSymbol == s2 asSymbol. "true"

(s1 class allInstances select: [:s | s = s1 ]) size. "2"
(s1 asSymbol class allInstances select: [:s | s = s1 asSymbol ]) size. "1"

"Comparing Symbols takes less time than comparing Strings"

[ #stringA = #stringB ] bench. "26,812,864 per second"
[ 'StringA' = 'StringB' ] bench. "3,492,987 per second"