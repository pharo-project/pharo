Class {
	#name : 'RBRenameMethodRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBRenameMethodRefactoringTest >> testFailureExistingSelector [
		
	| refactoring |
	refactoring := ReRenameMethodRefactoring renameMethod: #checkClass: in: RBBasicLintRuleTestData to: #runOnEnvironment: permutation: (1 to: 1).

	self
		should: [ refactoring generateChanges ]
		raise: RBRefactoringWarning 
]
