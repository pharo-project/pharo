Class {
	#name : 'RBPullUpInstanceVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBPullUpInstanceVariableRefactoringTest >> testShouldWarnWhenNotAllSubclassesDefineVariable [

	self addClassHierarchy.
	(model classNamed: #Subclass) addInstanceVariable: #result.
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #SomeClass;
			name: #SubclassWithoutResult;
			package: #'Refactory-Test data' ].
	self shouldWarn: (RePullUpInstanceVariableRefactoring
					 model: model
					 variable: #result
					 class: #SomeClass)
]
