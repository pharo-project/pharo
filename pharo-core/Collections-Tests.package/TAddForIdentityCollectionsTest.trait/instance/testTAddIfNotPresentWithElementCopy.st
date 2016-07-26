tests - adding for identity collections
testTAddIfNotPresentWithElementCopy
" test specific to IdentityCollections for wich #addIfNotPresent should use == check insted of =."
	| added oldSize collection element elementCopy |
	collection := self identityCollectionWithElementsCopyNotIdentical   .
	oldSize := collection  size.
	
	element := collection  anyOne .
	elementCopy := element copy.
	self deny: (collection  includes: elementCopy ).
	
	added := collection  addIfNotPresent: elementCopy  .
	self assert: added == elementCopy . "test for identiy because #add: has not reason to copy its parameter."
	self assert: (collection  size = (oldSize + 1)).

	