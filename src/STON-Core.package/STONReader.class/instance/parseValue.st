parsing
parseValue
	| value |
	value := self parseSimpleValue.
	^ (self matchChar: $:)
		ifTrue: [ STON associationClass key: value value: self parseValue ]
		ifFalse: [ value ]