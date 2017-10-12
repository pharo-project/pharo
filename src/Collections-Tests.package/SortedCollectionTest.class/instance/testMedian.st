tests - basic
testMedian
	"self run: #testMedian"
	"self debug: #testMedian"
	
	|aSortedCollection|
	aSortedCollection := (1 to: 10) asSortedCollection.
	self assert: aSortedCollection median=5.
	
	aSortedCollection := SortedCollection new.
	aSortedCollection add:'truite' ; add:'porcinet' ; add:'carpe'.
	self assert: (aSortedCollection median = 'porcinet').
	