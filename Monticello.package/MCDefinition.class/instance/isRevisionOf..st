comparing
isRevisionOf: aDefinition
	^ (aDefinition isKindOf: MCDefinition) and: [aDefinition description = self description]