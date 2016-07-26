tests - index access
testIndexOfStartingAt
	"self debug: #testLastIndexOf"
	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection 
			indexOf: element
			startingAt: 2
			ifAbsent: [ 99 ]) = 99.
	self assert: (collection 
			indexOf: element
			startingAt: 1
			ifAbsent: [ 99 ]) = 1.
	self assert: (collection 
			indexOf: self elementNotInForIndexAccessing
			startingAt: 1
			ifAbsent: [ 99 ]) = 99