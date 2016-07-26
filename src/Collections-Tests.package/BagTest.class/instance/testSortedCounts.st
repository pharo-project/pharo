tests
testSortedCounts
	"self run: #testSortedCounts"
	"self debug: #testSortedCounts" 
	
	| bag sortedCounts|
	bag := Bag new.
	bag add: '1' withOccurrences: 10.
	bag add: '2' withOccurrences: 1.
	bag add: '3' withOccurrences: 5.
	
	sortedCounts := bag sortedCounts.
	self assert: sortedCounts size = 3.
	
	self assert: sortedCounts first = (10->'1').
	self assert: sortedCounts second =  (5->'3').
	self assert: sortedCounts third =  (1->'2').	