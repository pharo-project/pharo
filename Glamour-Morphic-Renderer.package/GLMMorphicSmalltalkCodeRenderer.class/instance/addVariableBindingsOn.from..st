rendering
addVariableBindingsOn: aTextModel from: aPresentation

	aPresentation variableBindings do: [ :each | aTextModel addVariableBinding: each ].