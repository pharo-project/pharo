Class {
	#name : 'ReRemoveUnusedTemporaryVariableRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
ReRemoveUnusedTemporaryVariableRefactoringTest >> setUp [

	| package |
	super setUp.
	package := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	model := RBNamespace onEnvironment: package.
	model defineClass: [ :aBuilder | 
		aBuilder
			superclass: Object;
			name: #ReClassForTesting;
			slots: { #instVar };
			package: 'Refactoring-DataForTesting'].
	class := model classNamed: 'ReClassForTesting'
]

{ #category : 'tests' }
ReRemoveUnusedTemporaryVariableRefactoringTest >> testRemoveUnusedTempsWhenAllUnusedTempsExpectAllTempsRemoved [

	| source refactoring parseTree |
	source := 'm
	| one two |
	^ 1'.
	class compile: source classified: '#test data'.
	
	refactoring := ReRemoveUnusedTemporaryVariableRefactoring model: model inMethod: #m inClass: class name.
	refactoring generateChanges.
	
	parseTree := class parseTreeForSelector: #m.
	
	self assert: parseTree allTemporaryVariables size equals: 0.
	self
		assert: parseTree
		equals: (self parseMethod: 'm ^ 1')
]

{ #category : 'tests' }
ReRemoveUnusedTemporaryVariableRefactoringTest >> testRemoveUnusedTempsWhenNoUnusedTempsExpectNothingIsRemoved [

	| source refactoring parseTree |
	source := 'm
	| one two |
	one := 1.
	two := 2.
	^ one + two'.
	class compile: source classified: '#test data'.
	
	refactoring := ReRemoveUnusedTemporaryVariableRefactoring model: model inMethod: #m inClass: class name.
	refactoring generateChanges.
	
	parseTree := class parseTreeForSelector: #m.
	
	self assert: parseTree allTemporaryVariables size equals: 2
]

{ #category : 'tests' }
ReRemoveUnusedTemporaryVariableRefactoringTest >> testRemoveUnusedTempsWhenOneUnusedTempsExpectTempIsRemoved [

	| source refactoring parseTree |
	source := 'm
	| one two |
	one := 1.
	^ one'.
	class compile: source classified: '#test data'.
	
	refactoring := ReRemoveUnusedTemporaryVariableRefactoring model: model inMethod: #m inClass: class name.
	refactoring generateChanges.
	
	parseTree := class parseTreeForSelector: #m.
	
	self assert: parseTree allTemporaryVariables size equals: 1
]
