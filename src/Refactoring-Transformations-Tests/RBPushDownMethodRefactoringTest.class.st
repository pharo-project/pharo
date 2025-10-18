Class {
	#name : 'RBPushDownMethodRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBPushDownMethodRefactoringTest >> testFailurePushDownMethodOnNonAbstractClass [

	| refactoring |
	refactoring := RBPushDownMethodRefactoring
		               pushDown: #( #isArray )
		               from: 'Array'.

	self
		should: [ refactoring generateChanges ]
		raise: RBRefactoringWarning
]

{ #category : 'failure tests' }
RBPushDownMethodRefactoringTest >> testFailurePushDownMethodSubclassesReferToSelector [

	| refactoring |
	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #Superclass;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #Superclass;
				name: #Foo1;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #Superclass;
				name: #Foo2;
				package: #'Refactory-Test data' ].
	(model classNamed: #Superclass) compile: 'yourself ^1' classified: #( #accessing ).
	(model classNamed: #Foo1) compile: 'method1 ^super yourself' classified: #( #accessing ).

	refactoring := RBPushDownMethodRefactoring model: model pushDown: #( #yourself ) from: 'Superclass'.


	self should: [ refactoring generateChanges ] raise: RBRefactoringWarning
]
