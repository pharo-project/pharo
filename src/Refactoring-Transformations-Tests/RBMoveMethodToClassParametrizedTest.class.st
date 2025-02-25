Class {
	#name : 'RBMoveMethodToClassParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBMoveMethodToClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMoveMethodToClassRefactoring };
		addCase: { #rbClass -> RBMoveMethodToClassTransformation };
		yourself
]

{ #category : 'tests' }
RBMoveMethodToClassParametrizedTest >> constructor [
	^ #method:class:
]

{ #category : 'failure tests' }
RBMoveMethodToClassParametrizedTest >> testFailureMethodAlreadyDefined [

	| method someClass |
	someClass := model classNamed: #RBFooLintRuleTestData.
	method := model rbMethodClass
		          for: someClass
		          source: 'foo' , String cr , String tab , '^ 6'
		          selector: #foo.

	self shouldFail: (self createRefactoringWithArguments: {
				 method.
				 someClass })
]
