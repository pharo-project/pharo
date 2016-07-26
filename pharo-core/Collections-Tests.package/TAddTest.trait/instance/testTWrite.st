tests - adding
testTWrite
	| added collection elem |
	collection := self otherCollection  .
	elem := self element .
	added := collection  write: elem .
	
	self assert: added == elem .	"test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection  includes: elem )	.
	self assert: (collection  includes: elem ).
	
	