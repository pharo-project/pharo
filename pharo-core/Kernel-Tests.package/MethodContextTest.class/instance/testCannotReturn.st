tests
testCannotReturn
	| context p |
	p := [context := thisContext] fork.
	[p isTerminated] whileFalse: [ 10 milliSeconds wait ].

	[ context resume: #result.
	self assert: false description: 'ContextCannotReturn should be signaled' ]
		 on: ContextCannotReturn do: [ :err |
			self assert: err result equals: #result.
			self assert: err target equals: context]