parse
parseKeyword
	| keyword rangeIndices |
	self parseBinary.
	keyword := ''.
	rangeIndices := #().
	[[self isKeyword]
		whileTrue: [keyword := keyword , currentToken.
			self rangeType: #keyword.
			rangeIndices := rangeIndices copyWith: ranges size.
			self scanNext.
			self parseTerm.
			self parseBinary].
	nil]
		ensure: [| type | (keyword isEmpty
					or: [Symbol
							hasInterned: keyword
							ifTrue: [:sym | nil]])
				ifFalse: [type := (Symbol thatStartsCaseSensitive: keyword skipping: nil) isNil
								ifTrue: [#undefinedKeyword]
								ifFalse: [#incompleteKeyword].
					rangeIndices
						do: [:i | (ranges at: i)
								type: type]]]