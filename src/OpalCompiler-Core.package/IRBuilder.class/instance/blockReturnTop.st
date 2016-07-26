instructions
blockReturnTop
	| retInst |
	
	retInst := IRInstruction blockReturnTop.
	self add: retInst.
	self startNewSequence.
	retInst  successor: currentSequence.
	self popScope.