tests
testAsDateAndTime
	self assert: (aTime asDateAndTime) = (DateAndTime current midnight + aTime)
