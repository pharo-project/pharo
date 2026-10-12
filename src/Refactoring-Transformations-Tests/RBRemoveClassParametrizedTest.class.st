Class {
	#name : 'RBRemoveClassParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBRemoveClassParametrizedTest class >> testParameters [

	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> ReRemoveClassRefactoring .
					  #constructor -> #classNames: };
		addCase: { #rbClass -> ReRemoveClassesTransformation .
					  #constructor -> #classNames: };
		yourself
]

{ #category : 'running' }
RBRemoveClassParametrizedTest >> setUp [

	super setUp.
	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #DecoratorRemovableClass;
			package: #'Refactory-Test data' ]
]

{ #category : 'failure tests' }
RBRemoveClassParametrizedTest >> testApplicabilityPreconditionsExposeMissingClass [

	| refactoring failedPreconditions |
	refactoring := self createRefactoringWithModel: model andArguments:
		              { { #DecoratorMissingClass } }.
	refactoring prepareForExecution.

	self
		shouldnt: [
			failedPreconditions := refactoring failedApplicabilityPreconditions ]
		raise: MessageNotUnderstood.

	self assert: failedPreconditions size equals: 1.
	self assert:
		(failedPreconditions first isKindOf: ReClassesExistCondition)
]

{ #category : 'failure tests' }
RBRemoveClassParametrizedTest >> testFailureNonExistentClass [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { { #DecoratorMissingClass } })
]

{ #category : 'tests' }
RBRemoveClassParametrizedTest >> testRemoveUnreferencedLeafClass [

	self executeRefactoring: (self createRefactoringWithModel: model andArguments:
			 { { #DecoratorRemovableClass } }).
	self deny: (model includesClassNamed: #DecoratorRemovableClass)
]
