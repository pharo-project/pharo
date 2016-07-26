tests
testTo
	self assert: (aDateAndTime to: aDateAndTime) = ((DateAndTime year: 1970 month: 1 day: 1) to: (DateAndTime year: 1970 month: 1 day: 1)) 
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "