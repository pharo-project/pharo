Class {
	#name : 'RBPullUpMethodRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBPullUpMethodRefactoringTest >> testWarnWhenPullUpWillOverrideMethodFromSuperclass [

	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #SomeClass;
			sharedVariables: { #Foo };
			package: #'Refactory-Test data' ].
	(model classNamed: #SomeClass) compile: 'yourself ^54' classified: #( #accessing ).

	self shouldWarn: (RePullUpMethodRefactoring model: model pullUp: #( #yourself ) from: (model classNamed: #SomeClass))
]
