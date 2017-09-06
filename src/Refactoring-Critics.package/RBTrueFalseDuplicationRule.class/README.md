Checks for ifTrue:ifFalse: blocks that have the same code at the beginning or end. While you might not originally write such code, as it is modified, it is easier to create such code. Instead of having the same code in two places, you should move it outside the blocks.

For example, 
test 
	ifTrue: [self foo. self bar ] 
	ifFalse: [ self foo. self baz ]
 is equivalent to: 

self foo.  
test 
	ifTrue: [ self bar ] 
	ifFalse: [ self baz ]