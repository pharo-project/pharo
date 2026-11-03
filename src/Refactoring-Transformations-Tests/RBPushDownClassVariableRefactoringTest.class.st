Class {
	#name : 'RBPushDownClassVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBPushDownClassVariableRefactoringTest >> testShouldWarnWhenVariableReferencedInDefiningClass [

	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #PushDownClassVariableRoot;
				sharedVariables: { #Foo };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #PushDownClassVariableRoot;
				name: #PushDownClassVariableChild;
				package: #'Refactory-Test data' ].
	(model classNamed: #PushDownClassVariableRoot)
		compile: 'usesFoo ^ Foo'
		classified: #( #accessing ).
	self shouldWarn: (RBPushDownClassVariableRefactoring
					 model: model
					 variable: #Foo
					 class: #PushDownClassVariableRoot)
]
