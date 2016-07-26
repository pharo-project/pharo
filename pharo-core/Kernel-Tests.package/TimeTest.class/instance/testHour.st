tests
testHour
	self assert: aTime hour =  12.
	self assert: aTime hour12 =  12.
	self assert: aTime hour24 =  12.
	self assert: aTime hours =  12.