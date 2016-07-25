accessing
definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock
	| definition |
	definition := definitions at: aDefinition description ifAbsent: [].
	^ definition
		ifNil: errorBlock
		ifNotNil: [foundBlock value: definition]