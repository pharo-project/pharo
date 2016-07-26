tests - index access
testLastIndexOfIfAbsent
	"self debug: #testIndexOfIfAbsent"
	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection 
			lastIndexOf: element
			ifAbsent: [ 99 ]) = 1.
	self assert: (collection 
			lastIndexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 99 ]) = 99