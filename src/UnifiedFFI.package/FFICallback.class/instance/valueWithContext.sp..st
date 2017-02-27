evaluation
valueWithContext: callbackContext sp: stackPointer 
	| argumentValues |
	
	self flag: #todo. "This can be optimised in a shadow method"
	argumentValues := self argumentsFor: stackPointer context: callbackContext.
	^ functionSpec returnType 
		callbackReturnOn: callbackContext 
		for: (block valueWithArguments: argumentValues)