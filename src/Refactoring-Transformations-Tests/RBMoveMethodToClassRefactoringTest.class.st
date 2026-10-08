Class {
	#name : 'RBMoveMethodToClassRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'tests' }
RBMoveMethodToClassRefactoringTest >> testFailureMethodAlreadyDefined [

	| method targetClass sourceClass |
	sourceClass := model classNamed: #RBBasicLintRuleTestData.
	targetClass := model classNamed: #RBFooLintRuleTestData.
	sourceClass compile: 'foo' , String cr , String tab , '^ 42' classified: #testing.
	method := sourceClass methodFor: #foo.
	"refactoring should warn when method already exists."
	self shouldWarn: (ReMoveMethodToClassRefactoring method: method class: targetClass)
]
