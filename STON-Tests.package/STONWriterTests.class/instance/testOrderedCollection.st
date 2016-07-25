tests
testOrderedCollection
	| collection |
	collection := OrderedCollection with: 1 with: 2 with: 3.
	self assert: (self serialize: collection) = 'OrderedCollection[1,2,3]'.
	self assert: (self serialize: OrderedCollection new) = 'OrderedCollection[]'.