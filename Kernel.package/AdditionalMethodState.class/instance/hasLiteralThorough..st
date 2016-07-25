testing
hasLiteralThorough: literal
	"Answer true if any literal in these properties is literal,
	 even if embedded in array structure."
	1 to: self basicSize do: [:i |
		| propertyOrPragma "<Association|Pragma>" |
		propertyOrPragma := self basicAt: i.
		(propertyOrPragma isVariableBinding
			ifTrue: [propertyOrPragma key == literal
					or: [propertyOrPragma value == literal
					or: [propertyOrPragma value isArray
						and: [propertyOrPragma value hasLiteral: literal]]]]
			ifFalse: [propertyOrPragma hasLiteral: literal]) ifTrue:
			[^true]].
	^false