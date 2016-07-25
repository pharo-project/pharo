preconditions
checkVariableName
	(class whoDefinesInstanceVariable: newVariableName) notNil 
		ifTrue: 
			[self refactoringError: ('<1p> defines an instance variable named <2s>' 
						expandMacrosWith: class
						with: newVariableName)].
	(class whoDefinesClassVariable: newVariableName) notNil 
		ifTrue: 
			[self refactoringError: ('<1p> defines a class variabled named <2s>' 
						expandMacrosWith: class
						with: newVariableName)].
	(self parseTree allDefinedVariables includes: newVariableName) 
		ifTrue: 
			[self refactoringError: ('<1s> is already a temporary variable name' 
						expandMacrosWith: newVariableName)]