rendering
modelFor: aPresentation

	^ (super modelFor: aPresentation)
		bindingsStrategy: self methodBindingsStrategy;
		yourself