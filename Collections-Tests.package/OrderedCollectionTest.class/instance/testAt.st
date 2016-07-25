tests - accessing
testAt
	| collection |
	collection := #('Jim' 'Mary' 'John' 'Andrew' ) asOrderedCollection.
	self assert: (collection at:1) = 'Jim'.
	self assert: (collection at:2) = 'Mary'