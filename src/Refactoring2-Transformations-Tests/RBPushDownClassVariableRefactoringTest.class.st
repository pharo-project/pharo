Class {
	#name : 'RBPushDownClassVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring2-Transformations-Tests-Test',
	#package : 'Refactoring2-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBPushDownClassVariableRefactoringTest >> testFailureModelPushDownToMultipleSubclassesFailure [

	model defineClass: 'Object << #SomeClass
		sharedVariables: { #Foo } ;
		package: #''Refactory-Test data'''.
	model defineClass: 'SomeClass << #Subclass
		package: #''Refactory-Test data'''.
	(model classNamed: #Subclass)
		compile: 'foo ^Foo'
		classified: #( #accessing ).
	model defineClass: 'SomeClass << #AnotherSubclass
		package: #''Refactory-Test data'''.
	(model metaclassNamed: #AnotherSubclass)
		compile: 'bar ^Foo'
		classified: #( #accessing ).
	self shouldFail: (RBPushDownClassVariableRefactoring
			 model: model
			 variable: #Foo
			 class: #SomeClass)
]
