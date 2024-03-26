Class {
	#name : 'RBAddNewClassRefactoring',
	#superclass : 'RBInsertNewClassRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBAddNewClassRefactoring >> applicabilityPreconditions [

	^ { 
		(RBCondition isValidClassName: className) .
		(RBCondition isGlobal: className in: self model) not .
		(RBCondition isSymbol: packageName) .
		((RBCondition withBlock: [ packageName isNotEmpty ]) errorMacro: 'Invalid package name')
		}
]
