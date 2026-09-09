Class {
	#name : 'RBPullUpMethodRefactoringTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBPullUpMethodRefactoringTest >> setUp [

	super setUp.
	self addClassHierarchy
]

{ #category : 'tests' }
RBPullUpMethodRefactoringTest >> testPullUpMethod [

	| refactoring class |
	class := model classNamed: #Foo1.
	(model classNamed: #Foo1) compile: 'someMethod | temp | temp := 7. ^temp' classified: #( #accessing ).
	refactoring := RePullUpMethodRefactoring model: model pullUp: { #someMethod } from: class.
	self assert: (class directlyDefinesMethod: #someMethod).
	self proceedThroughWarning: [ self executeRefactoring: refactoring ].
	self deny: (class directlyDefinesMethod: #someMethod)
]

{ #category : 'failure tests' }
RBPullUpMethodRefactoringTest >> testWarnDoesntPullUpReferencesInstVar [

	| refactoring class |
	class := model classNamed:
		         ('RBTransformation' , 'RuleTestData1') asSymbol.
	refactoring := RePullUpMethodRefactoring
		               model: model
		               pullUp: #( #foo )
		               from: class.
	self shouldWarn: refactoring
]

{ #category : 'failure tests' }
RBPullUpMethodRefactoringTest >> testWarnPullUpWithInvalidSuperSend [

	| class |
	self addClassHierarchy.
	(model classNamed: #Foo2)
		compile: 'yourself ^super yourself + 1'
		classified: #( #accessing ).
	class := model classNamed: #Foo1.
	class compile: 'yourself ^1' classified: #( #accessing ).

	self shouldWarn:
		(RePullUpMethodRefactoring model: model pullUp: { #yourself } from: class)
]

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

{ #category : 'failure tests' }
RBPullUpMethodRefactoringTest >> testWarnWhenPulledMethodRefersToLocalSharedVar [

	| class |
	class := model classNamed: #Object.
	class addClassVariable: #Foo.
	class classSide compile: 'foo ^Foo' classified: #( #accessing ).
	self shouldWarn:
		(RePullUpMethodRefactoring model: model pullUp:  #( #foo ) from: class classSide)
]
