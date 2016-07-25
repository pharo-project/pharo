operations
removeDefinition: aDefinition
	index
		definitionLike: aDefinition
		ifPresent: [:other | 
			other = aDefinition
				ifTrue: [(self modificationConflictForDefinition: aDefinition)
							ifNotNil: [:c |
									c beNonConflict.
									^ self]. 
						(self redundantAdds includes: aDefinition)
							ifFalse: [self addOperation: (MCRemoval of: aDefinition)]]
				ifFalse: [self addConflictWithOperation: (MCRemoval of: other)]]
		ifAbsent: []