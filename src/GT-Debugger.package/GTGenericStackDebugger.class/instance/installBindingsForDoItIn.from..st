building presentations
installBindingsForDoItIn: aPresentation from: aContext

	(aContext notNil and: [aContext method isDoIt and: [ aContext receiver isNil ] ]) ifTrue: [
		aPresentation variableBindings: aContext gtDebuggerDoItBindings ]

	