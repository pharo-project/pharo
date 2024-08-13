Extension { #name : 'RBParseTreeRewriter' }

{ #category : '*Refactoring-Transformations' }
RBParseTreeRewriter class >> replaceStatements: code with: newCode in: aParseTree [

	| tree replaceStmt |
	tree := self buildTree: code method: false.
	tree isSequence
		ifFalse: [tree := RBSequenceNode statements: (Array with: tree)].
	tree temporaries: (Array with: (ASTPatternVariableNode named: '`@temps')).
	tree addNodeFirst: (ASTPatternVariableNode named: '`@.S1').
	tree lastIsReturn ifTrue: [replaceStmt := '| `@temps | `@.S1. ^' , newCode].

	replaceStmt := '| `@temps | `@.S1. ' , newCode.
	(tree statements last = aParseTree body statements last)
		ifFalse:
			[tree addNode: (ASTPatternVariableNode named: '`@.S2').
			replaceStmt := replaceStmt, '. `@.S2'].

	^self
		replace: tree formattedCode
		with: replaceStmt
		in: aParseTree
		onInterval: aParseTree sourceInterval
]
