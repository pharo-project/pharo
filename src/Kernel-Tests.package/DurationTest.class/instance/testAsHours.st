tests
testAsHours
	self assert: (Duration hours: 2) asHours = 2.
	self assert: (Duration days: 1) asHours = 24.	
	self assert: (aDuration asHours closeTo: 26.0511).