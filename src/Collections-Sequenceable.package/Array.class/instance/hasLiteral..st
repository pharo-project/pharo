private
hasLiteral: literal
	"Answer true if literal is identical to any literal in this array, even 
	if imbedded in further array structure. This method is only intended 
	for private use by CompiledMethod hasLiteralSymbol:"

	| lit |
	1 to: self size do: 
		[:index | 
		((lit := self at: index) literalEqual: literal) ifTrue: [^true].
		(Array == lit class and: [lit hasLiteral: literal]) ifTrue: [^true]].
	^false