tests - index access
testIdentityIndexOf
	"self debug: #testIdentityIndexOf"
	| collection element |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection identityIndexOf: element) = (collection indexOf: element)