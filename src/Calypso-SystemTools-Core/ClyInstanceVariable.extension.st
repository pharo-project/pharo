Extension { #name : #ClyInstanceVariable }

{ #category : #'*Calypso-SystemTools-Core' }
ClyInstanceVariable >> createRefactoring: variableRefactoringClass for: aClass [
	^actualVariable createRefactoring: variableRefactoringClass for: aClass
]
