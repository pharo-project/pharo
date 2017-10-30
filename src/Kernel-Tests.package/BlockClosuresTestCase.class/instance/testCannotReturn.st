tests
testCannotReturn

	| blk |
	blk := self constructCannotReturnBlockInDeadFrame.
	self 
		should: [blk value: 1]
		raise: Exception
