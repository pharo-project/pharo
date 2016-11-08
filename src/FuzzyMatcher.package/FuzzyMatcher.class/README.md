FuzzyMatcher is an approximate string matching algroithm that can determine if a string includes a given pattern.
For example, the string 'axby' matches both the pattern 'ab' and, 'ay', but not 'ba'. 

The algorithm is based on lib_fts[1], and includes an optional scoring algorithm that can be used to sort all the matches based on their similarity to the pattern.

1: 
https://blog.forrestthewoods.com/reverse-engineering-sublime-text-s-fuzzy-match-4cffeed33fdb
https://github.com/forrestthewoods/lib_fts
