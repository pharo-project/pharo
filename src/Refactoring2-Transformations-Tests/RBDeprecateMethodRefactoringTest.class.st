Class {
	#name : #RBDeprecateMethodRefactoringTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #'failure tests' }
RBDeprecateMethodRefactoringTest >> testShouldWarnWhenNewSelectorHasInvalidNumArgs [
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."

	| refactoring oldSelector newSelector |
	oldSelector := ('called:' , 'on1:') asSymbol.
	newSelector := 'inlineFoo:' asSymbol.
	refactoring := RBDeprecateMethodRefactoring
		               model: model
		               deprecateMethod: oldSelector
		               in: RBClassDataForRefactoringTest
		               using: newSelector.

	self
		should: [ refactoring generateChanges ]
		raise: RBRefactoringWarning 
]

{ #category : #'failure tests' }
RBDeprecateMethodRefactoringTest >> testShouldWarnWhenNewSelectorIsNotDefinedInClass [
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."

	| refactoring oldSelector newSelector |
	oldSelector := ('called:' , 'on1:') asSymbol.
	newSelector := 'inlineFoo1:' asSymbol.
	refactoring := RBDeprecateMethodRefactoring
		               model: model
		               deprecateMethod: oldSelector
		               in: RBClassDataForRefactoringTest
		               using: newSelector.

	self
		should: [ refactoring generateChanges ]
		raise: RBRefactoringWarning 
]
