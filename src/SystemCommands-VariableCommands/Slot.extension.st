Extension { #name : #Slot }

{ #category : #'*SystemCommands-VariableCommands' }
Slot >> createRefactoring: variableRefactoringClass for: declarationClass [
	^variableRefactoringClass 
			variable: self name
			class: declarationClass
]
