Extension { #name : #ClyVariable }

{ #category : #'*Calypso-SystemTools-Core' }
ClyVariable >> createVariableRefactoring: variableRefactoringClass [ 
	^self createRefactoring: variableRefactoringClass for: definingClass
]
