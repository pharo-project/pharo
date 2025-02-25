Class {
	#name : 'RBPushDownInstanceVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBPushDownInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBPushDownInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBPushDownVariableTransformation .
					  #constructor -> #instanceVariable:class: };
		yourself
]

{ #category : 'running' }
RBPushDownInstanceVariableParametrizedTest >> setUp [

	super setUp.
	model := self modelOnClasses: { RBBasicLintRuleTestData . RBLintRuleTestData }
]

{ #category : 'failure tests' }
RBPushDownInstanceVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { 'foo'. #RBBasicLintRuleTestData })
]

{ #category : 'tests' }
RBPushDownInstanceVariableParametrizedTest >> testModelPushDownOnMetaclassSide [

	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #SomeClass;
			package: #'Refactory-Test data' ].
	(model metaclassNamed: #SomeClass) addInstanceVariable: 'foo'.
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #SomeClass;
			name: #Subclass;
			package: #'Refactory-Test data' ].
	(model metaclassNamed: #Subclass) compile: 'foo ^foo' classified: #( #accessing ).
	self executeRefactoring: (self createRefactoringWithModel: model andArguments: { 'foo'. #'SomeClass class' }).
	self deny: ((model metaclassNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self assert: ((model metaclassNamed: #Subclass) directlyDefinesVariable: 'foo')
]

{ #category : 'tests' }
RBPushDownInstanceVariableParametrizedTest >> testModelPushDownToMultipleSubclasses [

	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #SomeClass;
				slots: { #foo };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #SomeClass;
				name: #Subclass;
				package: #'Refactory-Test data' ].
	(model classNamed: #Subclass) compile: 'foo ^foo' classified: #( #accessing ).
	model defineClass: [ :aBuilder |
		aBuilder
			superclassName: #SomeClass;
			name: #AnotherSubclass;
			package: #'Refactory-Test data' ].
	(model classNamed: #AnotherSubclass) compile: 'bar ^foo' classified: #( #accessing ).
	self executeRefactoring: (self createRefactoringWithModel: model andArguments: { 'foo'. #SomeClass }).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #AnotherSubclass) directlyDefinesVariable: 'foo')
]

{ #category : 'tests' }
RBPushDownInstanceVariableParametrizedTest >> testModelRemoveUnusedVariable [

	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #SomeClass;
				slots: { #foo };
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #SomeClass;
				name: #Subclass;
				package: #'Refactory-Test data' ].

	self executeRefactoring: (self createRefactoringWithModel: model andArguments: { 'foo'. #SomeClass }).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo')
]

{ #category : 'tests' }
RBPushDownInstanceVariableParametrizedTest >> testPushDownInstanceVariable [
	| refactoring |
	refactoring := self createRefactoringWithModel: model andArguments:
		{'foo1' . #RBLintRuleTestData}.
	self executeRefactoring: refactoring.
	(refactoring model classNamed: #RBLintRuleTestData) subclasses do:
		[ :each | self assert: (each directlyDefinesInstanceVariable: 'foo1') ]
]
