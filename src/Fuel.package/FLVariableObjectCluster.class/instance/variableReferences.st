accessing
variableReferences
	^ variableReferences ifNil: [ variableReferences := FLLargeIdentityDictionary new ]