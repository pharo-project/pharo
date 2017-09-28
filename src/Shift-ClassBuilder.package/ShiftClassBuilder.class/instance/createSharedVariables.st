building
createSharedVariables
	builderEnhancer 
		on: newClass
		declareClassVariables: self layoutDefinition sharedVariables
		sharing: self layoutDefinition sharedPoolsString.
	