tests
testTommorrow
	self assert: (DateAndTime today + 24 hours) =  (DateAndTime tomorrow).
	self deny: aDateAndTime =  (DateAndTime tomorrow).
     "MessageNotUnderstood: Date class>>starting:"