I represent  a two-dimensional grid.

I provide methods for creating grids, operating on them.

A grid origin is the left topmost corner, hence subsequent lines are located "below".

Examples
========

self exampleGrid22.
self exampleGrid22WithPointLocationCreatedWithRows.
self exampleGrid3x2BooksCreatedWithRows.
self exampleGrid6x2.
self exampleGrid6x2CreatedWithRowsColumns.

Structure
========

The implementation of this grid is not optimised for lines manipulation in the sense that it uses an array where lines are placed one after the other. 

 - numberOfRows : a non-negative integer saying how many rows there are.
 - numberOfColumns : a non-negative integer saying how many columns there are.
 - contents : an Array holding the elements in row-major order.  That is, for a 2x3 array the contents are (11 12 13 21 22 23).  

Todo
====
- should write tests for all the methods.
- it would be good to be able to say that when we create a grid with row 1 is at the bottom and not the top.  -> check if it is true.
- ordering lines based on sorted order of one element (should have a look at the shorting collection extension)
- swapping lines and rows
- columnsDo:
- rowsDo:
	
Not obvious that we want the following
	- hidding lines and rows	
