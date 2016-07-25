transforming
removeImmediateBlocks
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter 
		replace: '[``.object] value'
		with: '``.object'
		when: [:aNode | aNode parent isCascade not].
	rewriter 
		replace: '| `@temps | ``@.Stmts1. [| `@bTemps | ``@.bStmts] value. ``@.Stmts2'
		with: '| `@temps `@bTemps | ``@.Stmts1. ``@.bStmts. ``@.Stmts2'.
	(rewriter executeTree: sourceParseTree) 
		ifTrue: [sourceParseTree := rewriter tree]