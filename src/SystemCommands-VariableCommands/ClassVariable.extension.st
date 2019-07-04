Extension { #name : #ClassVariable }

{ #category : #'*SystemCommands-VariableCommands' }
ClassVariable >> createRefactoring: variableRefactoringClass for: declarationClass [
	^variableRefactoringClass classVariableAnalog
			variable: self name
			class: declarationClass
]
