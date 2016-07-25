transforming
inlineSourceReplacing: aParseTree 
	| statements nodeUnderSequence |
	statements := inlineParseTree body statements.
	(statements size > 1 and: [aParseTree isEvaluatedFirst not]) 
		ifTrue: 
			[self 
				refactoringWarning: 'To inline this method, we need to move some of its statements before the original message send.<n>This could change the order of execution, which can change the behavior.<n>Do you want to proceed?' 
						expandMacros].
	nodeUnderSequence := aParseTree.
	[nodeUnderSequence parent isSequence] 
		whileFalse: [nodeUnderSequence := nodeUnderSequence parent].
	(nodeUnderSequence parent)
		addNodes: (statements copyFrom: 1 to: (statements size - 1 max: 0))
			before: nodeUnderSequence;
		addTemporariesNamed: inlineParseTree body temporaryNames.
	aParseTree parent replaceNode: aParseTree
		withNode: (statements isEmpty 
				ifTrue: [RBVariableNode named: 'self']
				ifFalse: [statements last])