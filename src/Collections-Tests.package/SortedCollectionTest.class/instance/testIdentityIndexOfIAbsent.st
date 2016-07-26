tests - index access
testIdentityIndexOfIAbsent
	| collection element |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection 
			identityIndexOf: element
			ifAbsent: [ 0 ]) = 1.
	self assert: (collection 
			identityIndexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 55 ]) = 55