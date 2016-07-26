tests - index access
testLastIndexOf
	"self debug: #testLastIndexOf"
	| element collection |
	collection := self collectionMoreThan1NoDuplicates.
	element := collection first.
	self assert: (collection lastIndexOf: element) = 1.
	self assert: (collection lastIndexOf: self elementNotInForIndexAccessing) = 0