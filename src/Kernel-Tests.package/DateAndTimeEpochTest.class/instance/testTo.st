tests
testTo
	self assert: (aDateAndTime to: aDateAndTime) = (DateAndTime new to: DateAndTime new) 
	"MessageNotUnderstood: UndefinedObject>>starting:ending:  where UndefinedObject is Timespan "