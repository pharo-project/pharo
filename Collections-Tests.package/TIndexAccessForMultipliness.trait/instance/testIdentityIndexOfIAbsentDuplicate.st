tests - index accessing for multipliness
testIdentityIndexOfIAbsentDuplicate
	"self debug: #testIdentityIndexOfIfAbsent"
	| collection element elementCopy |
	collection := self collectionWithNonIdentitySameAtEndAndBegining .
	element := collection last.
	elementCopy := element copy.
	self deny: element  == elementCopy .
	self assert: (collection 
			identityIndexOf: element
			ifAbsent: [ 0 ]) = collection size.
	self assert: (collection 
			identityIndexOf: elementCopy
			ifAbsent: [ 55 ]) = 55