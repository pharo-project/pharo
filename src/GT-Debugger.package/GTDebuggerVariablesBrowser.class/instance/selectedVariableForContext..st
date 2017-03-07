accessing
selectedVariableForContext: aContext 
	^ (self variablesFromContext: aContext) 
		detect: [ :each | each key = self debugger selectedVariableName ] 
		ifNone: [ cachedVariables detect: [ :each | each key = 'self' ] ifNone: [ nil ] ]