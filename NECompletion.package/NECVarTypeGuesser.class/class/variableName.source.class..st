instance creation
variableName: aString source: sourceString class: aClass 
	| newInstance |
	newInstance := self basicNew initialize.
	newInstance 
		setVariableName: aString
		source: sourceString
		class: aClass.
	^ newInstance