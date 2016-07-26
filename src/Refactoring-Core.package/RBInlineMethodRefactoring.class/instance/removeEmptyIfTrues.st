transforming
removeEmptyIfTrues
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter
		replace: '``@boolean ifTrue: [] ifFalse: [| `@temps | ``@.Stmts]'
			with: '``@boolean ifFalse: [|`@temps | ``@.Stmts]';
		replace: '``@boolean ifFalse: [] ifTrue: [| `@temps | ``@.Stmts]'
			with: '``@boolean ifTrue: [|`@temps | ``@.Stmts]';
		replace: '``@boolean ifTrue: [| `@temps | ``@.Stmts] ifFalse: []'
			with: '``@boolean ifTrue: [|`@temps | ``@.Stmts]';
		replace: '``@boolean ifFalse: [| `@temps | ``@.Stmts] ifTrue: []'
			with: '``@boolean ifFalse: [|`@temps | ``@.Stmts]'.
	(rewriter executeTree: sourceParseTree) 
		ifTrue: [sourceParseTree := rewriter tree]