tests
testToBy
	self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days) = 
				((DateAndTime year: 1980 month: 1 day: 1) to:
				 (DateAndTime year: 1980 month: 1 day: 1) + 10 days by: 5 days ) 
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "