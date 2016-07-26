serialize/materialize
initializeAnalyzing

	notIgnoredVariables := self instanceVariableNamesToSerialize.
	
	mapping := notIgnoredVariables 
		collect: [ :name | theClass instVarIndexFor: name ].