parse
parseUnary
	| unary type |
	[self isName]
		whileTrue: [unary := currentToken.
			type := #unary.
			(unary isEmpty
					or: [Symbol
							hasInterned: unary
							ifTrue: [:sym | nil]])
				ifFalse: [type := (Symbol thatStartsCaseSensitive: unary skipping: nil) isNil
								ifTrue: [#undefinedUnary]
								ifFalse: [#incompleteUnary]].
			self scanPast: type]