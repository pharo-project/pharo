Class {
	#name : 'RBMoveMethodToClassParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring2-Transformations-Tests-Parametrized',
	#package : 'Refactoring2-Transformations-Tests',
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
	method := RBClassModelFactory rbMethod
		          for: someClass
		          source: 'foo' , String cr , String tab , '^ 6'
		          selector: #foo.

	self shouldFail: (self createRefactoringWithArguments: {
				 method.
				 someClass })
]
