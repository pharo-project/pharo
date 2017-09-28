accessing
sharedVariables: aCollection
	self layoutDefinition sharedVariables: (aCollection collect:#asClassVariable).