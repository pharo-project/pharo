parse
parseStringOrSymbol
	currentTokenFirst == $'
		ifTrue: [^ self parseString].
	currentTokenFirst == $#
		ifTrue: [^ self parseSymbol].
	self error