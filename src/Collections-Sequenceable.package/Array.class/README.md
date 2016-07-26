Array is a fixed-sized collection of elements accessed by integer indices. Contrary to the C convention, the first element of a Smalltalk array is at position 1 and not 0. The main protocol to access array elements is the method at: and at:put:. 

- at: anInteger returns the element at index anInteger. 
- at: anInteger put: anObject puts anObject at index anInteger. 

Arrays are fixed-size collections therefore we cannot add or remove elements at the end of an array. 

The following code creates an array of size 5, puts values in the first 3 locations and returns the first element.

[[[ 
| anArray |
anArray := Array new: 5. 
anArray at: 1 put: 4. 
anArray at: 2 put: 3/2. 
anArray at: 3 put: 'ssss'. 
anArray at: 1			
> 4
]]]

#() creates literal arrays with static (or “literal”) elements that have to be known when the expression is compiled, and not when it is executed. 

The following code creates an array of size 2 where the first element is the (literal) number 1 and the second the (literal) string 'here'.

[[[  
#(1 'here') size 
> 2
]]]

{ } is a way to create arrays in a more dynamic manner.

[[[ 
| array |
array := (Array new: 2). array
	at: 1 put: (Point x: 10 y: 20);
	at: 2 put: (Point x: 10 y: 20). 
	array			
]]]

is equivalent to 

[[[
{(10@20) . (10@20)}
{Point x: 10 y: 20 . Point x: 10 y: 20}
]]]
