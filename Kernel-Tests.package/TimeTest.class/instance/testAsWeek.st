tests
testAsWeek
	self assert: aTime asWeek = (DateAndTime current midnight + aTime) asWeek
