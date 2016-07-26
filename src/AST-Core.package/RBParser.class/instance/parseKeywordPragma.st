private-parsing
parseKeywordPragma
	| selector positions arguments |
	selector := ''.
	positions := OrderedCollection new: 2.
	arguments := OrderedCollection new: 2.
	[ currentToken isKeyword ] whileTrue: [
		selector := selector, currentToken value.
		positions add: currentToken start.
		self step.
		arguments addLast: self parsePragmaLiteral ].
	^ self pragmaNodeClass
		selector: selector asSymbol
		keywordsPositions: positions asIntegerArray
		arguments: arguments.