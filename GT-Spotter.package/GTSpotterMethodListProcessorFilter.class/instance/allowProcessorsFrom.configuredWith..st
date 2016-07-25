adding
allowProcessorsFrom: aMethod configuredWith: aBlock
	
	self allowProcessorsFrom: aMethod.
	configurationBlocks at: aMethod printString put: aBlock.
	