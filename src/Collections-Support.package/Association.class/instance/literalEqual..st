testing
literalEqual: otherLiteral
	"Answer true if the receiver and otherLiteral represent the same literal.
	Variable bindings are literally equals only if identical.
	This is how variable sharing works, by preserving identity and changing only the value."
	^self == otherLiteral