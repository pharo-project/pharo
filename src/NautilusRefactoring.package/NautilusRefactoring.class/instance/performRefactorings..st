performing
"protocol: performing"
performRefactorings: aCollectionRefactoring
	aCollectionRefactoring ifNil: [ ^ self ].
	aCollectionRefactoring do: [ :each | each model environment: self model browsedEnvironment ].
	[ 
	[ 
	[ self internalPerformRefactorings: aCollectionRefactoring ]
		on: RBRefactoringFailure
		do: [ :exception | self handleFailure: exception ] ]
		on: RBRefactoringWarning
		do: [ :exception | self handleWarning: exception ] ]
		on: RBRefactoringError
		do: [ :exception | self handleError: exception ]