querying
type

	type == nil ifTrue: [ type := self extractTypeFromDescription ].
	^type