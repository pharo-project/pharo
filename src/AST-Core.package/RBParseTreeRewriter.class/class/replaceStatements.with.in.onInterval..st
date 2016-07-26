accessing
replaceStatements: code with: newCode in: aParseTree onInterval: anInterval 
	| tree replaceStmt |
	tree := self buildTree: code method: false.
	tree isSequence 
		ifFalse: [tree := RBSequenceNode statements: (Array with: tree)].
	tree temporaries: (Array with: (RBPatternVariableNode named: '`@temps')).
	tree addNodeFirst: (RBPatternVariableNode named: '`@.S1').
	tree lastIsReturn 
		ifTrue: [replaceStmt := '| `@temps | `@.S1. ^' , newCode]
		ifFalse: 
			[tree addNode: (RBPatternVariableNode named: '`@.S2').
			replaceStmt := '| `@temps | `@.S1. ' , newCode , '. `@.S2'].
	^self 
		replace: tree formattedCode
		with: replaceStmt
		in: aParseTree
		onInterval: anInterval