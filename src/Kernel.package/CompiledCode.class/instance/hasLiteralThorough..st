literals
hasLiteralThorough: literal
	"Answer true if any literal in this method is literal,
	even if embedded in array structure."

	(self penultimateLiteral isMethodProperties
	 and: [self penultimateLiteral hasLiteralThorough: literal]) ifTrue:[^true].
	2 to: self numLiterals - 1 "exclude superclass + selector/properties"
	   do:[:index | | lit |
		(((lit := self objectAt: index) literalEqual: literal)
		 or: [lit isArray and: [lit hasLiteral: literal]]) ifTrue:
			[^ true]].
	^ false 