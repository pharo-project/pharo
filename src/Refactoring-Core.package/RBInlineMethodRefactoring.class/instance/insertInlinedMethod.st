transforming
insertInlinedMethod
	| node |
	node := sourceMessage.
	self moveComments.
	node parent isCascade 
		ifTrue: 
			[self rewriteCascadedMessage.
			node := node parent].
	node parent isReturn 
		ifTrue: [node := node parent]
		ifFalse: [self removeReturns].
	self replaceArguments.
	self inlineSourceReplacing: node.
	sourceParseTree removeDeadCode.
	self removeEmptyIfTrues.
	self removeImmediateBlocks