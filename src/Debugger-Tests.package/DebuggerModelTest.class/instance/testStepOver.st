tests
testStepOver

	self assert: session interruptedContext equals: context.
	self assert: session interruptedContext printString equals: '[ Set new ] in DebuggerModelTest>>setUp' .

	session stepOver.

	self assert: session interruptedContext printString equals: '[ Set new ] in DebuggerModelTest>>setUp'.