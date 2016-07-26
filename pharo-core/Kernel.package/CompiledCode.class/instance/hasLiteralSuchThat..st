literals
hasLiteralSuchThat: litBlock
	"Answer true if litBlock returns true for any literal in this method, even if embedded in array structure."
	(self penultimateLiteral isMethodProperties
	 and: [self penultimateLiteral hasLiteralSuchThat: litBlock]) ifTrue:
		[^true].
	2 to: self numLiterals + 1 do:
		[:index | | lit |
		lit := self objectAt: index.
		((litBlock value: lit)
		or: [lit isArray and: [lit hasLiteralSuchThat: litBlock]]) ifTrue:
			[^true]].
	^false