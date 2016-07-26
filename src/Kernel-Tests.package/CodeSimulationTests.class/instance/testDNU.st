tests
testDNU

	self should: [ self runSimulated: [self absentMethod] ] raise: MessageNotUnderstood