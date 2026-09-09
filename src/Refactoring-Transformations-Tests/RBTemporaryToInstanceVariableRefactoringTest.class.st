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
	(model classNamed: #Foo) compile: 'someMethod | foo | foo := 4. ^foo' classified: #( #accessing ).
	(model classNamed: #Foo1) compile: 'someMethod2 | temp | temp := 7. ^temp' classified: #( #accessing ).
	(model classNamed: #Foo1) compile: 'someMethod3 | temp2 | ^temp2' classified: #( #accessing ).
	(model classNamed: #Bar) compile: 'tempMethod | bar | bar := 10. ^bar' classified: #( #accessing ).
	(model classNamed: #Bar) compile: 'tempMethod2| bar| bar := -6. ^bar' classified: #( #accessing )
]

{ #category : 'tests' }
RBTemporaryToInstanceVariableRefactoringTest >> testFailureOtherMethodDefinesTemporaryVariable [

	| class |
	class := model classNamed: #Bar.
	self
		should: [ (ReTemporaryToInstanceVariableRefactoring class: class selector: #tempMethod variable: 'bar') generateChanges ]
		raise: RBRefactoringWarning
]

{ #category : 'tests' }
RBTemporaryToInstanceVariableRefactoringTest >> testFailureTemporaryVariableReadBeforeWritten [

	| class |
	class := model classNamed: #Foo1.
	self
		should: [ (ReTemporaryToInstanceVariableRefactoring class: class selector: #someMethod3 variable: 'temp2') generateChanges ]
		raise: RBRefactoringWarning
]

{ #category : 'tests' }
RBTemporaryToInstanceVariableRefactoringTest >> testTemporaryVariableUsedInSingleMethod [

	| class |
	class := model classNamed: #Foo1.
	self
		shouldnt: [ (ReTemporaryToInstanceVariableRefactoring class: class selector: #someMethod2 variable: 'temp') generateChanges ]
		raise: RBRefactoringWarning
]
