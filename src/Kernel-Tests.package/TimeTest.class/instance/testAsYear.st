tests
testAsYear
	self assert: aTime asYear = (DateAndTime current midnight + aTime) asYear
