Class {
	#name : 'RBRemoveInstanceVariable2ParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBRemoveInstanceVariable2ParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> ReRemoveInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		yourself
]

{ #category : 'running' }
RBRemoveInstanceVariable2ParametrizedTest >> setUp [

	super setUp.
	model := self modelOnClasses: { RBLintRuleTestData }
]

{ #category : 'failure tests' }
RBRemoveInstanceVariable2ParametrizedTest >> testFailureNonExistantName [

	self shouldFail:
		(self createRefactoringWithModel: model andArguments:
			 { 'name1'. #RBLintRuleTestData })
]

{ #category : 'tests' }
RBRemoveInstanceVariable2ParametrizedTest >> testRemoveLocallyDefinedInstanceVariable [

	| refactoring class |
	refactoring := self createRefactoringWithModel: model andArguments: {#foo1 . #RBLintRuleTestData}.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class definesInstanceVariable: #foo1).

	self executeRefactoring: refactoring.

	self deny: (class definesInstanceVariable: #foo1)
]

{ #category : 'tests' }
RBRemoveInstanceVariable2ParametrizedTest >> testRemoveNonLocalInstanceVariableProducesAnError [

	| subclass subsubclass refactoring |
	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #ObjectSubclass;
				slots: { #foo1 };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #ObjectSubclass;
				name: #ObjectSubsubclass;
				package: #'Refactory-Test data' ].

	subclass := model classNamed: #ObjectSubclass.
	subsubclass := model classNamed: #ObjectSubsubclass.

	self assert: (subclass definesInstanceVariable: #foo1).
	self assert: (subclass directlyDefinesInstanceVariable: #foo1).

	self assert: (subsubclass definesInstanceVariable: #foo1).
	self deny: (subsubclass directlyDefinesInstanceVariable: #foo1).

	refactoring := self createRefactoringWithModel: model andArguments: { #foo1. #ObjectSubsubclass }.

	self should: [ refactoring checkPreconditions ]
		raise: RBRefactoringError, RBRefactoringWarning.
	self should: [ self executeRefactoring: refactoring ] raise: refactoring refactoringErrorClass.

	self deny: (subsubclass directlyDefinesInstanceVariable: #foo1).
	self assert: (subsubclass definesInstanceVariable: #foo1).
	self assert: (subclass definesInstanceVariable: #foo1)
]

{ #category : 'failure tests' }
RBRemoveInstanceVariable2ParametrizedTest >> testShouldWarnWhenVariableReferenced [

	self shouldWarn: (self createRefactoringWithModel: model andArguments:  { 'name'. #RBLintRuleTestData })
]

{ #category : 'tests' }
RBRemoveInstanceVariable2ParametrizedTest >> testVerifyPreconditionWhenRemoveLocallyDefinedInstanceVariable [

	| class refactoring |
	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #ObjectSubclass;
			slots: { #foo1 };
			package: #'Refactory-Test data' ].

	class := model classNamed: #ObjectSubclass.
	self assert: (class definesInstanceVariable: #foo1).
	self assert: (class directlyDefinesInstanceVariable: #foo1).

	refactoring := self createRefactoringWithModel: model andArguments: { #foo1. #ObjectSubclass }.
	self shouldnt: [ refactoring checkPreconditions ]
		raise: RBRefactoringError, RBRefactoringWarning.

	self executeRefactoring: refactoring.

	self deny: (class definesInstanceVariable: #foo1)
]
