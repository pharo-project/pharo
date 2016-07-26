tests - index access
testIndexOfIfAbsent
	"self debug: #testIndexOfIfAbsent"
	| collection |
	collection := self collectionMoreThan1NoDuplicates.
	self assert: (collection 
			indexOf: collection first
			ifAbsent: [ 33 ]) = 1.
	self assert: (collection 
			indexOf: self elementNotInForIndexAccessing
			ifAbsent: [ 33 ]) = 33