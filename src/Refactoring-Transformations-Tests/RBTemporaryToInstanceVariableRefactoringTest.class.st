Class {
	#name : 'RBTemporaryToInstanceVariableRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBTemporaryToInstanceVariableRefactoringTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest.
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #Foo;
			name: #Foo1;
			slots: { #foo };
			package: #'Refactory-Test data' ].
	(model classNamed: #Foo)
		compile: 'someMethod | foo | foo := 4. ^foo'
		classified: #( #accessing )
]

{ #category : 'tests' }
RBTemporaryToInstanceVariableRefactoringTest >> testFailureHierarchyDefinesVarableNamedAsTemporary [

	| class |
	class := model classNamed: #Foo.
	self
		should: [
			(RBTemporaryToInstanceVariableRefactoring
				 class: class
				 selector: #someMethod
				 variable: 'foo') generateChanges ]
		raise: RBRefactoringWarning
]
