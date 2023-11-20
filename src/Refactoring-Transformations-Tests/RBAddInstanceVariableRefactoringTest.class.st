Class {
	#name : 'RBAddInstanceVariableRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBAddInstanceVariableRefactoringTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'failure tests' }
RBAddInstanceVariableRefactoringTest >> testFailureAlreadyExistingName [

	self shouldFail: (RBAddInstanceVariableRefactoring
			 variable: 'class'
			 class: #RBTransformationRuleTestData).
	self shouldFail: (RBAddInstanceVariableRefactoring
			 variable: 'name'
			 class: #RBTransformationRuleTestData)
]

{ #category : 'failure tests' }
RBAddInstanceVariableRefactoringTest >> testFailureModelAlreadyExistingName [

	self shouldFail: (RBAddInstanceVariableRefactoring
			 model: model
			 variable: 'instVarName1'
			 class: #Bar)
]

{ #category : 'failure tests' }
RBAddInstanceVariableRefactoringTest >> testFailureModelMetaclassAlreadyExistingName [

	(model metaclassNamed: #Foo) addInstanceVariable: 'instVarName1'.

	self shouldFail: (RBAddInstanceVariableRefactoring
			 model: model
			 variable: 'instVarName1'
			 class: #'Bar class')
]

{ #category : 'failure tests' }
RBAddInstanceVariableRefactoringTest >> testFailureNewExistingName [

	| refactoring |
	model := RBNamespace new.
	model name: 'Add instance variable'.
	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #FOOBAR;
			slots: { #asdf };
			package: #'Refactory-Test data' ].
	refactoring := RBAddInstanceVariableRefactoring model: model variable: 'asdf' class: #FOOBAR.

	self shouldFail: refactoring
]

{ #category : 'failure tests' }
RBAddInstanceVariableRefactoringTest >> testFailureNewHierarchyExistingName [

	| refactoring |
	model := RBNamespace new.
	model name: 'Add instance variable'.

	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #FOOBAR;
			slots: { #asdf };
			package: #'Refactory-Test data' ].

	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #FOOBAR;
			name: #BARFOO;
			package: 'Refactory-Test data' ].

	refactoring := RBAddInstanceVariableRefactoring model: model variable: 'asdf' class: #BARFOO.

	self shouldFail: refactoring
]
