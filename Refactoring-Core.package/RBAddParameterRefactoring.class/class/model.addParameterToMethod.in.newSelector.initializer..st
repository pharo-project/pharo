instance creation
model: aRBSmalltalk addParameterToMethod: aSelector in: aClass newSelector: newSelector initializer: init 
	^(self new)
		model: aRBSmalltalk;
		addParameterToMethod: aSelector
			in: aClass
			newSelector: newSelector
			initializer: init;
		yourself