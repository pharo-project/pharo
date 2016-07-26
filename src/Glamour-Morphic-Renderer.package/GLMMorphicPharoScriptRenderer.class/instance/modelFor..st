rendering
modelFor: aPresentation

	^ (super modelFor: aPresentation)
		bindingsStrategy: self workspaceBindingsStrategy;
		appendVariableBinding: aPresentation variableBindings;
		yourself