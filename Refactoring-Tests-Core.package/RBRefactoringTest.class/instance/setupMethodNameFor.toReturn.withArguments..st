set up
setupMethodNameFor: aRefactoring toReturn: aSelector withArguments: stringCollection 
	| options |
	options := aRefactoring options copy.
	options at: #methodName
		put: 
			[:ref :aMethodName | 
			aMethodName
				selector: aSelector;
				arguments: stringCollection;
				yourself].
	aRefactoring options: options