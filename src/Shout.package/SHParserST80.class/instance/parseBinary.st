parse
parseBinary
	| binary type |
	self parseUnary.
	[self isBinary]
		whileTrue: [binary := currentToken.
			type := #binary.
			(binary isEmpty
					or: [Symbol
							hasInterned: binary
							ifTrue: [:sym | nil]])
				ifFalse: [type := (Symbol thatStartsCaseSensitive: binary skipping: nil) isNil
								ifTrue: [#undefinedBinary]
								ifFalse: [#incompleteBinary]].
			self scanPast: type.
			self parseTerm.
			self parseUnary]