testing
testMethodTemp
	| block1 block2 |
	block1 := self methodArgument: 1.
	block2 := self methodArgument: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2