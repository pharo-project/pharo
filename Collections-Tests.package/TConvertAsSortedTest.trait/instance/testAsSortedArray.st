tests - as sorted collection
testAsSortedArray
	| result collection |
	collection := self collectionWithSortableElements .
	result := collection  asArray sort.
	self assert: (result class includesBehavior: Array).
	self assert: result isSorted.
	self assert: result size = collection size