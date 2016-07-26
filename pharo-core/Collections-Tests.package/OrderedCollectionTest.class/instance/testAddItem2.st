tests - adding
testAddItem2
	| collection |
	collection := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
	collection add: 'James' before: 'Jim'.
	collection add: 'Margaret' before: 'Andrew'.
	self assert: (collection indexOf: 'James')
			+ 1
			= (collection indexOf: 'Jim').
	self assert: (collection indexOf: 'Margaret')
			+ 1
			= (collection indexOf: 'Andrew')