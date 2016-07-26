instructions
storeTemp: aSelector
	| anInstruction |
	anInstruction := super storeTemp: aSelector.
	self rememberReference: anInstruction to: aSelector in: temps.