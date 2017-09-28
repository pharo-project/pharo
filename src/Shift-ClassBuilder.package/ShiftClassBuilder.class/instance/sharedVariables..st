accessing
sharedVariables: aCollection
	self layoutDefinition sharedVariables: (aCollection collect:[:e | e asClassVariable]).