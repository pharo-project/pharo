instructions
pushTemp: aSelector
	| anInstruction |
	anInstruction := super pushTemp: aSelector.
	self rememberReference: anInstruction to: aSelector in: temps.