preconditions
signalReferenceError: errorString 
	class realClass isNil 
		ifTrue: [ self refactoringError: errorString ]
		ifFalse: [| classVarName error |
			error := '<1s><n>Browse references?' expandMacrosWith: errorString.
			classVarName := variableName asSymbol.
			self 
				refactoringError: error
				with: [ self openBrowserOn: (RBVariableEnvironment 
						referencesToClassVariable: classVarName
						in: class realClass)]]