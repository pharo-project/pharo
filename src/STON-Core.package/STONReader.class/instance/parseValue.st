parsing
parseValue
	| value |
	value := self parseSimpleValue.
	^ (self matchChar: $:)
		ifTrue: [ 
			(self isValidPropertyNameType: value)
				ifFalse: [ self error: 'unexpected property name type' ].
			STON associationClass key: value value: self parseValue ]
		ifFalse: [ value ]