transforming
writeGuardClauses
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter
		replaceMethod: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [| `@t1 | `@.Stmts1. ^`@r1]. `@.s2. ^`@r2'
			with: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [| `@t1 | `@.Stmts1. ^`@r1] ifFalse: [`@.s2. ^`@r2]';
		replaceMethod: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifFalse: [| `@t1 | `@.Stmts1. ^`@r1]. `@.s2. ^`@r2'
			with: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [`@.s2. ^`@r2] ifFalse: [| `@t1 | `@.Stmts1. ^`@r1]';
		replaceMethod: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [| `@t1 | `@.Stmts1. ^`@r1]. `@.s2'
			with: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [| `@t1 | `@.Stmts1. ^`@r1] ifFalse: [`@.s2. ^self]';
		replaceMethod: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifFalse: [| `@t1 | `@.Stmts1. ^`@r1]. `@.s2'
			with: '`@methodName: `@args | `@temps | `@.s1. `@boolean ifTrue: [`@.s2. ^self] ifFalse: [| `@t1 | `@.Stmts1. ^`@r1]'.
	[rewriter executeTree: inlineParseTree] 
		whileTrue: [inlineParseTree := rewriter tree]