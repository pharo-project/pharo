tests - basic
testAdd
	"self run: #testAdd"
	"self debug: #testAdd"
	
	| collection |
	collection := #(10 9 8 7 5 4 4 2) asSortedCollection.
	self assert: collection first = 2.
	self assert: collection last = 10.
	self assert: (collection size = 8).
	collection add:1.
	self assert: (collection size = 9).
	collection add: 6.
	self assert: ((collection at: 5) = 5).
	self assert: (collection size = 10).
	collection add: 3.
	