tests - evaluating
testValueWithPossibleArgument
	| block  blockWithArg blockWith2Arg |

	block := [1].
	blockWithArg  := [:arg | arg].
	blockWith2Arg := [:arg1 :arg2 | {arg1. arg2}].

	self assert: (block valueWithPossibleArgument: 1) = 1.
	
	self assert: (blockWithArg valueWithPossibleArgument: 1) = 1.
	
	self assert: (blockWith2Arg valueWithPossibleArgument: 1) = {1 . nil}.
	

	