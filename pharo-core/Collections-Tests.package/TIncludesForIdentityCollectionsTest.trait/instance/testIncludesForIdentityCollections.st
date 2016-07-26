tests - including for identity collections
testIncludesForIdentityCollections
	"self debug: #testIncludesElementIsThere"
	| collection element elementCopy |
	collection := self identityCollectionWithElementsCopyNotIdentical .
	element := collection anyOne.
	elementCopy := element copy.
	self assert: (collection includes: element).
	self deny: (collection includes: elementCopy)