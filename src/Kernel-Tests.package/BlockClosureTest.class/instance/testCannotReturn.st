tests - evaluating
testCannotReturn

	| block p |
	p := [block := self blockWithNonLocalReturn: #result] fork.
	[p isTerminated] whileFalse: [ 10 milliSeconds wait ].
 
	[ block value.	
	self assert: false description: 'BlockCannotReturn should be signaled' ]
		 on: BlockCannotReturn do: [ :err |
			self assert: err result equals: #result.
			self assert: err home equals: block home]