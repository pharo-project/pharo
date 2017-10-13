tests - basic
testCreation
	"self run: #testCreation"
	"self debug: #testCreation"
	
	| collection |
	collection := #(10 9 3 6 1 8 7 5 4 2) asSortedCollection.
	self assert: collection = (1 to: 10) asSortedCollection.
	