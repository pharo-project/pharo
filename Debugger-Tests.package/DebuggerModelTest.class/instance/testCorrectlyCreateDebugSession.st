tests
testCorrectlyCreateDebugSession

	self assert: session isNotNil.	
	self assert: process isSuspended.
	self assert: session interruptedContext equals: context.
	self assert: session interruptedContext printString equals: '[ Set new ] in DebuggerModelTest>>setUp'.