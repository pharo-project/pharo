tests - adding
testTAddIfNotPresentWithNewElement

	| added oldSize collection elem |
	collection := self otherCollection .
	oldSize := collection  size.
	elem := self element .
	self deny: (collection  includes: elem ).
	
	added := collection  addIfNotPresent: elem .
	self assert: added == elem . "test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection  size = (oldSize + 1)).

	