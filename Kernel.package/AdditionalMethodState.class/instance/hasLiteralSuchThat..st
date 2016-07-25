testing
hasLiteralSuchThat: aBlock
	"Answer true if litBlock returns true for any literal in this array, even if embedded in further array structure.
	 This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"
	1 to: self basicSize do: [:i |
		| propertyOrPragma "<Association|Pragma>" |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
			ifTrue: [(aBlock value: propertyOrPragma key)
					or: [(aBlock value: propertyOrPragma value)
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteralSuchThat: aBlock]]]]
			ifFalse: [propertyOrPragma hasLiteralSuchThat: aBlock]) ifTrue:
			[^true]].
	^false