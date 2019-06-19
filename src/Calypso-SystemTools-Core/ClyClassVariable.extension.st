Extension { #name : #ClyClassVariable }

{ #category : #'*Calypso-SystemTools-Core' }
ClyClassVariable >> createRefactoring: variableRefactoringClass for: aClass [
	^variableRefactoringClass classVariableAnalog
			variable: actualVariable name
			class: aClass
]
