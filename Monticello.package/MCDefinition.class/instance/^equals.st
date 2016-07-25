comparing
= aDefinition
	^(aDefinition isKindOf: MCDefinition) and: [self isRevisionOf: aDefinition]