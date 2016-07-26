tests - evaluating
testValueWithPossibleArgs
	| block blockWithArg blockWith2Arg |
	block := [ 1 ].
	blockWithArg := [ :arg | arg ].
	blockWith2Arg := [ :arg1 :arg2 | 
	{arg1.
	arg2} ].
	self assert: (block valueWithPossibleArgs: #()) = 1.
	self assert: (block valueWithPossibleArgs: #(1)) = 1.
	self assert: (blockWithArg valueWithPossibleArgs: #()) isNil.
	self assert: (blockWithArg valueWithPossibleArgs: #(1)) = 1.
	self assert: (blockWithArg valueWithPossibleArgs: #(1 2)) = 1.
	self
		assert:
			(blockWith2Arg valueWithPossibleArgs: #())
				=
					{nil.
					nil}.
	self
		assert:
			(blockWith2Arg valueWithPossibleArgs: #(1))
				=
					{1.
					nil}.
	self assert: (blockWith2Arg valueWithPossibleArgs: #(1 2)) = #(1 2).
	self assert: (blockWith2Arg valueWithPossibleArgs: #(1 2 3)) = #(1 2)