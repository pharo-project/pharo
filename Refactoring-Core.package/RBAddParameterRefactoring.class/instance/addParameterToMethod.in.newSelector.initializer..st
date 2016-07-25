initialize-release
addParameterToMethod: aSelector in: aClass newSelector: newSel initializer: init 
	self 
		renameMethod: aSelector
		in: aClass
		to: newSel
		permutation: (1 to: newSel numArgs).
	initializer := init