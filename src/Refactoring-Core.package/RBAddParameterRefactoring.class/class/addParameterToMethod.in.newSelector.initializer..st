instance creation
addParameterToMethod: aSelector in: aClass newSelector: newSelector initializer: init 
	^self new addParameterToMethod: aSelector
		in: aClass
		newSelector: newSelector
		initializer: init