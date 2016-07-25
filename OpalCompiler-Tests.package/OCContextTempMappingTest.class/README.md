This testcase tests that we can correctly get the value of the temp vars from a context. That is,

 - it finds the correct context where the variable is defined to get the value
 - it handles well copying and temp vectors

Probably tests are missing covering all possible combinations of 
  - temps
  - copying temps
  - temp vectors
  - nested blocks
  - blocks from dead contexts
  - optimized blocks

However this class is a starting point for this.