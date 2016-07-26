callbacks
actOnVariableBindingsAccessRequest: ann 

	ann action ifNotNil: [ :aBlock |
		aBlock cull: textModel variableBindings ]