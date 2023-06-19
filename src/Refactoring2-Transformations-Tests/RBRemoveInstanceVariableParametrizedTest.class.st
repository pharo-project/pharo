Class {
	#name : #RBRemoveInstanceVariableParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #tests }
RBRemoveInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRemoveInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		yourself
]

{ #category : #'failure tests' }
RBRemoveInstanceVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail:
		(self createRefactoringWithArguments:
			 { 'name1'. #RBLintRuleTestData })
]

{ #category : #'failure tests' }
RBRemoveInstanceVariableParametrizedTest >> testFailureReferencedVariable [

	self shouldWarn:
		(self createRefactoringWithArguments:
			 { 'name'. #RBLintRuleTestData })
]

{ #category : #tests }
RBRemoveInstanceVariableParametrizedTest >> testRemoveLocallyDefinedInstanceVariable [

	| refactoring class |
	refactoring := self createRefactoringWithArguments: {#foo1 . #RBLintRuleTestData}.
	class := refactoring model classNamed: #RBLintRuleTestData.
	self assert: (class definesInstanceVariable: #foo1).

	self executeRefactoring: refactoring.

	self deny: (class definesInstanceVariable: #foo1)
]

{ #category : #tests }
RBRemoveInstanceVariableParametrizedTest >> testRemoveNonLocalInstanceVariableProducesAnError [
	
	| subclass subsubclass refactoring |
	
	model defineClass: 'Object << #ObjectSubclass
		slots: {#foo1};
		package: #''Refactory-Test data'''.
	model defineClass: 'ObjectSubclass << #ObjectSubsubclass
		package: #''Refactory-Test data'''.

	subclass := model classNamed: #ObjectSubclass.
	subsubclass := model classNamed: #ObjectSubsubclass.
	
	self assert: (subclass definesInstanceVariable: #foo1).
	self assert: (subclass directlyDefinesInstanceVariable: #foo1).

	self assert: (subsubclass definesInstanceVariable: #foo1).
	self deny: (subsubclass directlyDefinesInstanceVariable: #foo1).

	refactoring := (self createRefactoringWithModel: model andArguments: { #foo1 . #ObjectSubsubclass }).

	self deny: refactoring preconditions check.
	self 
		should: [self executeRefactoring: refactoring] 
		raise: refactoring refactoringErrorClass.

	self deny: (subsubclass directlyDefinesInstanceVariable: #foo1).
	self assert: (subsubclass definesInstanceVariable: #foo1).
	self assert: (subclass definesInstanceVariable: #foo1)
]

{ #category : #tests }
RBRemoveInstanceVariableParametrizedTest >> testVerifyPreconditionWhenRemoveLocallyDefinedInstanceVariable [

	| class refactoring |
	model defineClass: 'Object << #ObjectSubclass
		slots: {#foo1};
		package: #''Refactory-Test data'''.

	class := model classNamed: #ObjectSubclass.
	self assert: (class definesInstanceVariable: #foo1).
	self assert: (class directlyDefinesInstanceVariable: #foo1).

	refactoring := (self createRefactoringWithModel: model andArguments: { #foo1 . #ObjectSubclass }).
	self assert: refactoring preconditions check.
	
	self executeRefactoring: refactoring.

	self deny: (class definesInstanceVariable: #foo1)
]
