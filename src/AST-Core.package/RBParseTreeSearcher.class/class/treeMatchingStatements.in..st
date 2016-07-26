accessing
treeMatchingStatements: aString in: aParseTree 
	| notifier tree |
	notifier := self new.
	tree := RBParser parseExpression: aString.
	tree isSequence 
		ifFalse: [tree := RBSequenceNode statements: (Array with: tree)].
	tree temporaries: (Array with: (RBPatternVariableNode named: '`@temps')).
	tree addNodeFirst: (RBPatternVariableNode named: '`@.S1').
	tree lastIsReturn 
		ifFalse: [tree addNode: (RBPatternVariableNode named: '`@.S2')].
	notifier matchesTree: tree
		do: [:aNode :answer | ^RBParser parseExpression: aString].
	notifier executeTree: aParseTree.
	^nil