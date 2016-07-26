instruction decoding
methodReturnTop
	| last |
	last := stack removeLast "test test" asReturnNode.
	stack size > blockStackBase  "get effect of elided pop before return"
		ifTrue: [statements addLast: stack removeLast].
	exit := pc.
	lastJumpPc := lastReturnPc := lastPc.
	statements addLast: last