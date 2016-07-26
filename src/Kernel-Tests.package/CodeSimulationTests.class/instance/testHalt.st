tests
testHalt
	self should: [ self runSimulated: [self methodWithHalt] ] raise: Halt