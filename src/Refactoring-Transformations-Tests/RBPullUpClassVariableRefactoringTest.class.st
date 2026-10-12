Class {
	#name : 'RBPullUpClassVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBPullUpClassVariableRefactoringTest >> testShouldWarnWhenNotAllSubclassesDefineVariable [

	self addClassHierarchy.
	(model classNamed: #Subclass) addClassVariable: #Foo.
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #SomeClass;
			name: #SubclassWithoutFoo;
			package: #'Refactory-Test data' ].
	self shouldWarn: (RePullUpSharedVariableRefactoring
					 model: model
					 variable: #Foo
					 class: #SomeClass)
]
