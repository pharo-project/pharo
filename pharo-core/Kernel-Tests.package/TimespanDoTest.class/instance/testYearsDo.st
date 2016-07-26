tests
testYearsDo
	| yearArray |
	yearArray := Array
				with: (Year
						starting: (DateAndTime year: 2003 day: 7)
						duration: 365 days).
	self assert: aTimespan years contents = yearArray contents