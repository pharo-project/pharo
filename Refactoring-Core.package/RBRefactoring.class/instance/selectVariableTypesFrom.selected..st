requests
selectVariableTypesFrom: initialTypeCollection selected: selectedTypeCollection 
	^ (self options at: #variableTypes) 
		value: self
		value: initialTypeCollection
		value: selectedTypeCollection
