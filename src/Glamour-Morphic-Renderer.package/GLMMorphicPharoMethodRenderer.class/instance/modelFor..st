rendering
modelFor: aPresentation

	^ (super modelFor: aPresentation)
		bindingsStrategy: self methodBindingsStrategy;
		appendVariableBinding: aPresentation variableBindings;
		yourself