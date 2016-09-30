literals
hasLiteral: literal
	"Answer whether the receiver references the argument, literal."
	2 to: self numLiterals - 1 do: "exclude superclass + selector/properties"
		[:index |
		| lit |
		((lit := self objectAt: index) literalEqual: literal) ifTrue: [^true].
		(lit isMemberOf: CompiledBlock) ifTrue: [ (lit hasLiteral: literal) ifTrue: [ ^true ] ]].
	^false