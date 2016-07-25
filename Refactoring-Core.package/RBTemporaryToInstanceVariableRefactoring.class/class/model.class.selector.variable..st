instance creation
model: aRBSmalltalk class: aClass selector: aSelector variable: aVariableName 
	^(self new)
		model: aRBSmalltalk;
		class: aClass
			selector: aSelector
			variable: aVariableName;
		yourself