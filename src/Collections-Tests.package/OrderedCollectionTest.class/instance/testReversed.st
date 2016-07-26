tests - copying
testReversed
	| collection1 collection2 |
	collection1 := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
	collection2 := collection1 reversed.
	self assert: collection2 first = 'Andrew'.
	self assert: collection2 last = 'Jim'