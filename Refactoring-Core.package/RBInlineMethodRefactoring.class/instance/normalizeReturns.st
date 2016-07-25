transforming
normalizeReturns
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter
		replace: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ^``@r1] ifFalse: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ^``@r1] ifTrue: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ^``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]'
			with: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ^``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]'
			with: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '``@boolean ifTrue: [| `@t1 | `@.Stmts1. ^``@r1] ifFalse: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifTrue: [| `@t1 | `@.Stmts1. ``@r1] ifFalse: [| `@t2 | `@.Stmts2. ``@r2]';
		replace: '``@boolean ifFalse: [| `@t1 | `@.Stmts1. ^``@r1] ifTrue: [| `@t2 | `@.Stmts2. ^``@r2]'
			with: '^``@boolean ifFalse: [| `@t1 | `@.Stmts1. ``@r1] ifTrue: [| `@t2 | `@.Stmts2. ``@r2]'.
	[rewriter executeTree: inlineParseTree] 
		whileTrue: [inlineParseTree := rewriter tree]