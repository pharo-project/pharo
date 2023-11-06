Class {
	#name : 'RBPushDownClassVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBPushDownClassVariableRefactoringTest >> testFailureModelPushDownToMultipleSubclassesFailure [

	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #SomeClass;
				sharedVariables: { #Foo };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #SomeClass;
				name: #Subclass;
				package: #'Refactory-Test data' ].
	(model classNamed: #Subclass) compile: 'foo ^Foo' classified: #( #accessing ).
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #SomeClass;
			name: #AnotherSubclass;
			package: #'Refactory-Test data' ].
	(model metaclassNamed: #AnotherSubclass) compile: 'bar ^Foo' classified: #( #accessing ).
	self shouldFail: (RBPushDownClassVariableRefactoring model: model variable: #Foo class: #SomeClass)
]
