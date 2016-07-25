tests - adding
testTAddWithOccurences
	| added oldSize collection anElement |
	collection := self collectionWithElement .
	anElement := self element .
	oldSize := collection  size.
	added := collection  add: anElement withOccurrences: 5.
	
	self assert: added == anElement.	"test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection  includes: anElement).
	self assert: collection  size = (oldSize + 5)