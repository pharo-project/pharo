building
selectedVariableForContext: aContext 
	|variablesMappings|
	
	variablesMappings := self variablesFromContext: aContext.
	^ variablesMappings 
		detect: [ :each | each key = self debugger selectedVariableName ] 
		ifNone: [ variablesMappings detect: [ :each | each key = 'self' ] ifNone: [ nil ] ]