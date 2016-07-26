tests - adding
testTAddTwice
	| added oldSize collection anElement |
	collection := self collectionWithElement .
	anElement := self element .
	oldSize := collection  size.
	added := collection 
		add: anElement ;
		add: anElement .
	self assert: added == anElement .	"test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection  includes: anElement ).
	self assert: collection  size = (oldSize + 2)