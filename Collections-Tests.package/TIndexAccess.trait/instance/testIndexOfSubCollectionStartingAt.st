tests - index access
testIndexOfSubCollectionStartingAt
	"self debug: #testIndexOfIfAbsent"
	| subcollection index collection |
	collection := self collectionMoreThan1NoDuplicates.
	subcollection := self collectionMoreThan1NoDuplicates.
	index := collection 
		indexOfSubCollection: subcollection
		startingAt: 1.
	self assert: index = 1.
	index := collection 
		indexOfSubCollection: subcollection
		startingAt: 2.
	self assert: index = 0