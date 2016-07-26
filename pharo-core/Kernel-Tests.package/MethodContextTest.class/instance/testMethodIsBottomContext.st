tests
testMethodIsBottomContext
	self assert: aMethodContext bottomContext = aSender.
	self assert: aMethodContext secondFromBottom = aMethodContext.