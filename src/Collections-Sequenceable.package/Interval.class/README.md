I represent a finite arithmetic progression (a range of number).

Description
--------------------------

I allow to iterate easily on a range of number (for example to manupulate an index) with a define step (by default one by one).

Zero step size is not allowed and will raise an error.

I know at which number I begin, at which number I end and the step.

I work with the Number class. I manipulate some numbers and I can be created from a Number.  

Public API and Key Messages
--------------------------

- I implement most of the classic Iterators as #do: or #collect:.

- #from: to: and #from: to: by:  are my two common contructors. But I am usually created by a message send on Number  (See examples).

Examples 
--------------------------

To create an Interval from 1 to 100 there is many ways:

	Interval from: 1 to: 100
	or
	Interval from: 1 to: 100 by: 1
	
	or from a Number 
	
	1 to: 100 
	or 
	1 to: 100 by: 1
	
	You can also use floats or fractions: 
	
	0.1 to: 0.5 by: 0.01
	or
	1/10 to: 1/2 by: 1/100
	
	NB: both expressions will not give exactly the same result. The first will contains only floats and the second only fractions.
	
 
Internal Representation and Key Implementation Points.
--------------------------

    Instance Variables
	start:		<Number> 	The beginning of the Interval.
	step:		<Number> 	The end of the Interval.
	stop:		<Number> 	The step of the interval. If the step is 3 and we begin at 1 the interval will be 1, 4, 7, 10, 13… until the end.
