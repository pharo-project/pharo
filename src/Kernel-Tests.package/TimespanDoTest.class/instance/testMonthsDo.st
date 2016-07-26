tests
testMonthsDo
	| monthArray |
	monthArray := Array
				with: (Month
						starting: (DateAndTime year: 2003 day: 1)
						duration: 31 days)
				with: (Month
						starting: (DateAndTime year: 2003 day: 32)
						duration: 28 days)
				with: (Month
						starting: (DateAndTime year: 2003 day: 60)
						duration: 31 days)		
				with: (Month
						starting: (DateAndTime year: 2003 day: 91)
						duration: 30 days).
	self assert: aTimespan months = monthArray