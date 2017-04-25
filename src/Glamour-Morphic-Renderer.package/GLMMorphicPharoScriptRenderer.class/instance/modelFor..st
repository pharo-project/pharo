rendering
modelFor: aPresentation

	^ (super modelFor: aPresentation)
		highlightSmalltalk: true;
		bindingsStrategy: self workspaceBindingsStrategy;
		appendVariableBinding: aPresentation variableBindings;
		yourself