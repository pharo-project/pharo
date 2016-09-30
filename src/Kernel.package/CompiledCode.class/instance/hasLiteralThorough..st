literals
hasLiteralThorough: literal
	"Answer true if any literal in this method is literal,
	even if embedded in array structure."

	(self penultimateLiteral isMethodProperties
	 and: [self penultimateLiteral hasLiteralThorough: literal]) ifTrue:[^true].
	2 to: self numLiterals - 1 "exclude header and methodClass or outerCode"
	   do:[:index | | lit |
		(((lit := self objectAt: index) literalEqual: literal)
		 or: [lit isArray and: [lit hasLiteral: literal]]) ifTrue:
			[^ true].
		(lit isMemberOf: CompiledBlock) ifTrue: [ (lit hasLiteralThorough: literal) ifTrue: [ ^true ] ] ].
	^ false 