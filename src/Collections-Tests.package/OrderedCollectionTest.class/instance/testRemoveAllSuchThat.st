tests - removing
testRemoveAllSuchThat
	| collection |
	collection := (1 to: 10) asOrderedCollection.
	collection
		removeAllSuchThat: [:e | e even].
	self assert: collection = (1 to: 10 by: 2) asOrderedCollection