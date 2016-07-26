operations
modificationConflictForDefinition: aDefinition
	^ self conflicts 
		detect: [:ea |
			(ea definition isRevisionOf: aDefinition) and:
				[ea operation isModification]] 
		ifNone: []