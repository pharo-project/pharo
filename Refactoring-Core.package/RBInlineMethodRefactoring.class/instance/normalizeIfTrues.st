transforming
normalizeIfTrues
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter
		replace: '| `@temps | ``@.s1. ``@boolean ifTrue: [| `@t1 | ``@.Stmts1. ^`@r1]. ``@.s2. ^``@r2'
			with: '| `@temps | ``@.s1. ``@boolean ifTrue: [| `@t1 | ``@.Stmts1. ^`@r1] ifFalse: [``@.s2. ^``@r2]';
		replace: '| `@temps | ``@.s1. ``@boolean ifFalse: [| `@t1 | ``@.Stmts1. ^`@r1]. ``@.s2. ^``@r2'
			with: '| `@temps | ``@.s1. ``@boolean ifTrue: [``@.s2. ^``@r2] ifFalse: [| `@t1 | ``@.Stmts1. ^`@r1]'.
	[rewriter executeTree: inlineParseTree] 
		whileTrue: [inlineParseTree := rewriter tree]