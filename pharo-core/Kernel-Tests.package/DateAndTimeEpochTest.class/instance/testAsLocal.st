tests
testAsLocal
	self assert: aDateAndTime asLocal =  aDateAndTime.
	self assert: aDateAndTime asLocal = (aDateAndTime offset: aDateAndTime class localOffset)
	
