operations
modifyDefinition: baseDefinition to: targetDefinition
	index
		definitionLike: baseDefinition
		ifPresent: [:other | 
			other = baseDefinition
				ifTrue: [self addOperation: (MCModification of:  baseDefinition to: targetDefinition)]
				ifFalse: [other = targetDefinition
							ifFalse: [self addConflictWithOperation:
											(MCModification of: other to: targetDefinition)]]]
		ifAbsent: [self addConflictWithOperation: (MCAddition of: targetDefinition)]