Class {
	#name : #RBPushDownMethodRefactoringTest,
	#superclass : #RBAbstractTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #'failure tests' }
RBPushDownMethodRefactoringTest >> testFailurePushDownMethodOnNonAbstractClass [

	| refactoring |
	refactoring := RBPushDownMethodRefactoring
		               pushDown: #( #isArray )
		               from: Array.
	"Scripting API"
	self shouldFail: refactoring.
	
	"Driver API"
	self proceedThroughWarning: [
		self
			should: [ refactoring generateChanges ]
			raise: RBBreakingChangeChecksFailedWarning
	]
]

{ #category : #'failure tests' }
RBPushDownMethodRefactoringTest >> testFailurePushDownMethodSubclassesReferToSelector [

	| refactoring |
	model defineClass:
		'Object << #Superclass package: #''Refactory-Test data'''.
	model defineClass:
		'Superclass << #Foo1 package: #''Refactory-Test data'''.
	model defineClass:
		'Superclass << #Foo2 package: #''Refactory-Test data'''.
	(model classNamed: #Superclass)
		compile: 'yourself ^1'
		classified: #( #accessing ).
	(model classNamed: #Foo1)
		compile: 'method1 ^super yourself'
		classified: #( #accessing ).

	refactoring := RBPushDownMethodRefactoring
		               model: model
		               pushDown: #( #yourself )
		               from: (model classNamed: #Superclass).
	"Scripting API"
	self shouldFail: refactoring.
	
	"Driver API"
	self proceedThroughWarning: [
		self
			should: [ refactoring generateChanges ]
			raise: RBBreakingChangeChecksFailedWarning
	]
]
