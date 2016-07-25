tests
testSortedElements
	"self run: #testSortedElements" 
	"self debug: #testSortedElements"
	
	| bag sortedElements|
	bag := Bag new.
	bag add: '2' withOccurrences: 1.
	bag add: '1' withOccurrences: 10.
	bag add: '3' withOccurrences: 5.
	
	sortedElements := bag sortedElements.
	
	self assert: sortedElements size = 3.
	
	self assert: sortedElements first = ('1'->10).
	self assert: sortedElements second =  ('2'->1).
	self assert: sortedElements third =  ('3'->5).
	