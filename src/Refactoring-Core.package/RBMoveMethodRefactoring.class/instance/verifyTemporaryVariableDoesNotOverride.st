transforming
verifyTemporaryVariableDoesNotOverride
	(parseTree allDefinedVariables includes: selfVariableName)
		ifTrue: [ ^ false ].
	^ moveToClasses 
		noneSatisfy: [ :each | each definesVariable: selfVariableName ]