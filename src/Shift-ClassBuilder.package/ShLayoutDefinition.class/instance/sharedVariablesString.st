accessing
sharedVariablesString
	^ (sharedVariables collect:[:e | e key]) joinUsing: ' '.