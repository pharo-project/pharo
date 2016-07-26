tests
testWhile
	| myFlag |
	self should: [[ nil ] whileFalse: [myFlag := true ]] raise: MessageNotUnderstood.