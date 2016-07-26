tests
testToBy
	self assert: (aDateAndTime to: aDateAndTime + 10 days by: 5 days) = 
				(DateAndTime new to: DateAndTime new + 10 days by: 5 days ) 
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "