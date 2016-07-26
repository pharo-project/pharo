tests - adding
testTWriteTwice
	| added oldSize collection elem |
	collection := self collectionWithElement .
	elem := self element .
	oldSize := collection  size.
	added := collection 
		write: elem ;
		write: elem .
	self assert: added == elem .	"test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection  includes: elem ).
	self assert: collection  size = (oldSize + 2)