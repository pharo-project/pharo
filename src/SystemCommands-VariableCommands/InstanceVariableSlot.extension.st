Extension { #name : #InstanceVariableSlot }

{ #category : #'*SystemCommands-VariableCommands' }
InstanceVariableSlot >> createRefactoring: variableRefactoringClass for: declarationClass [
	^variableRefactoringClass 
			variable: self name
			class: declarationClass
]
