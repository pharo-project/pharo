Class {
	#name : #RBRenameMethodRefactoringTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #'failure tests' }
RBRenameMethodRefactoringTest >> testFailureExistingSelector [
		
	| refactoring |
	refactoring := RBRenameMethodRefactoring renameMethod: #checkClass: in: RBBasicLintRuleTestData to: #runOnEnvironment: permutation: (1 to: 1).
	
	self
		should: [ refactoring generateChanges ]
		raise: RBBreakingChangeChecksFailedWarning 
]
