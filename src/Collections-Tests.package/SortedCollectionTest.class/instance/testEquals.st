tests - basic
testEquals
	"self run: #testEquals"
	"self debug: #testEquals"
	
	|aSortedCollection|
	aSortedCollection := SortedCollection new.
	aSortedCollection add:'truite' ; add: 'brochet'.
	self assert: aSortedCollection copy = aSortedCollection.