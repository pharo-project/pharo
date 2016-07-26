error handling
parseErrorNode: aMessageString
	| sourceString |
	currentToken isError 
		ifTrue: [ ^ RBParseErrorNode errorMessage: currentToken cause value: currentToken value at: currentToken start ].
	sourceString := source copyFrom: self errorPosition to: source size.
	^ RBParseErrorNode
		errorMessage: aMessageString value: sourceString at: self errorPosition