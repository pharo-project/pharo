tests
testCumulativeCounts
	"self run: #testCumulativeCounts" 
	"self debug: #testCumulativeCounts"
		
	| bag cumulativeCounts |
	bag := Bag new.
	bag add: '1' withOccurrences: 50.
	bag add: '2' withOccurrences: 40.
	bag add: '3' withOccurrences: 10.
	
	cumulativeCounts := bag cumulativeCounts.
	
	self assert: cumulativeCounts size = 3.
	self assert: cumulativeCounts first = (50 -> '1').
	self assert: cumulativeCounts second = (90 -> '2').
	self assert: cumulativeCounts third = (100 -> '3').
