tests - index access
testIndexOfSubCollectionStartingAtIfAbsent
	"self debug: #testIndexOfIfAbsent"
	| index absent subcollection collection |
	collection := self collectionMoreThan1NoDuplicates.
	subcollection := self collectionMoreThan1NoDuplicates.
	absent := false.
	index := collection 
		indexOfSubCollection: subcollection
		startingAt: 1
		ifAbsent: [ absent := true ].
	self assert: absent = false.
	absent := false.
	index := collection 
		indexOfSubCollection: subcollection
		startingAt: 2
		ifAbsent: [ absent := true ].
	self assert: absent = true