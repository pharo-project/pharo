tests
testStepInto

	self deny: (session isContextPostMortem: context).
	self assert: session interruptedContext equals: context.
	self assert: session interruptedContext printString equals: '[ Set new ] in DebuggerModelTest>>setUp' .

	session 
		stepInto;
		stepInto.

	self assert: session interruptedContext printString equals: 'Set class>>new'.
