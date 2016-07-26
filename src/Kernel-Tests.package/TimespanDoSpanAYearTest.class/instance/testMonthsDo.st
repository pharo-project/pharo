tests
testMonthsDo

	| monthArray |

	monthArray := Array
				with: (Month starting: (DateAndTime year: 2004 day: 355) duration: 31 days)
				with: (Month starting: (DateAndTime year: 2005 day: 1) duration: 31 days)
				with: (Month starting: (DateAndTime year: 2005 day: 32) duration: 29 days)
				with: (Month starting: (DateAndTime year: 2005 day: 61) duration: 31 days).
				
	self assert: aTimespan months = monthArray