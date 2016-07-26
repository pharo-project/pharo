tests - index access
testLastIndexOfStartingAt
	"self debug: #testLastIndexOf"
	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection last.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size
			ifAbsent: [ 99 ]) = collection size.
	self assert: (collection 
			lastIndexOf: element
			startingAt: collection size - 1
			ifAbsent: [ 99 ]) = 99.
	self assert: (collection 
			lastIndexOf: self elementNotInForIndexAccessing
			startingAt: collection size
			ifAbsent: [ 99 ]) = 99