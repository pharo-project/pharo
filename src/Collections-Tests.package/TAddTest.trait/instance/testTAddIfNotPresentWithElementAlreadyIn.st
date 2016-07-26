tests - adding
testTAddIfNotPresentWithElementAlreadyIn

	| added oldSize collection anElement |
	collection := self collectionWithElement .
	oldSize := collection size.
	anElement := self element .
	self assert: (collection  includes: anElement ).
	
	added := collection  addIfNotPresent: anElement .
	
	self assert: added == anElement .	"test for identiy because #add: has not reason to copy its parameter."
	self assert: collection  size = oldSize