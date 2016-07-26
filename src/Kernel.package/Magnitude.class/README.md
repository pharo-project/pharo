I'm the abstract class Magnitude that provides common protocol for objects that have
the ability to be compared along a linear dimension, such as dates or times.
Subclasses of Magnitude include Date, ArithmeticValue, and Time, as well as
Character and LookupKey.
 
 
My subclasses should implement
  < aMagnitude 
  = aMagnitude 
  hash

Here are some example of my protocol:
     3 > 4
     5 = 6
     100 max: 9
	7 between: 5 and: 10 
