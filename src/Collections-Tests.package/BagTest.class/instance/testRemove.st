tests
testRemove
	"self run: #testRemove"
	"self debug: #testRemove" 
	
	| bag item |
	item := 'test item'.
	bag := Bag new. 
	
	bag add: item.
	self assert: (bag size) = 1.
	bag remove: item.
	self assert: bag isEmpty.
	
	bag add: item withOccurrences: 2.
	bag remove: item.
	bag remove: item.
	self assert: (bag size) = 0.
	
	self should: [bag remove: item.] raise: Error.