actions
setBindings: aDictionary

	self inspectorPresentation firstPresentation 
		variableBindings: [ aDictionary associations ];
		updateVariableBindings