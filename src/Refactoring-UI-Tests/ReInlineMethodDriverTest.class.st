Class {
	#name : 'ReInlineMethodDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'model',
		'environment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'calculating' }
ReInlineMethodDriverTest >> intervalForMessageSend: aString inMethod: aSelector ofClass: aRBClass [

    | source start |
    source := (aRBClass methodFor: aSelector) source.
    start := source indexOfSubCollection: aString.
    ^ start to: start + aString size - 1
]

{ #category : 'initialization' }
ReInlineMethodDriverTest >> setUpDataForTesting [

    environment := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
    model := RBNamespace onEnvironment: environment.

    model
        defineClass: [ :b | b superclass: Object; name: #RBInlineBase; package: #'Refactory-Test data' ];
        defineClass: [ :b | b superclassName: #RBInlineBase; name: #RBInlineSub; package: #'Refactory-Test data'];
		  defineClass: [ :b | b superclassName: #RBInlineSub; name: #RBInlineSubSub; package: #'Refactory-Test data' ].

    (model classNamed: #RBInlineBase)
        compile: 'foo ^ 42'
        classified: #( #accessing ).


    (model classNamed: #RBInlineBase)
        compile: 'caller ^ self foo'
        classified: #( #accessing ).
]

{ #category : 'tests' }
ReInlineMethodDriverTest >> testInlineMethodDriverWhenExecutionOrderChangedExpectBreakingChange [

	| driver interval |
	self setUpDataForTesting.

	(model classNamed: #RBInlineBase) compile: 'foo | x | x := 1. x := x + 1. ^ x' classified: #( #accessing ).

	(model classNamed: #RBInlineBase) compile: 'callerLate | y | y := 0. y := self foo. ^ y' classified: #( #accessing ).

	interval := self intervalForMessageSend: 'self foo' inMethod: #callerLate ofClass: (model classNamed: #RBInlineBase).
	driver := ReInlineMethodDriver new
		          scopes: { environment }
		          interval: interval
		          selector: #callerLate
		          class: (model classNamed: #RBInlineBase).

	self setUpDriver: driver.
	driver runRefactoring.

	self assert: driver refactoring failedBreakingChangePreconditions size equals: 1.
	self assert: (driver refactoring failedBreakingChangePreconditions first isKindOf: ReInlineMethodExecutionOrderCondition)
]

{ #category : 'tests' }
ReInlineMethodDriverTest >> testInlineMethodDriverWhenMethodOverriddenExpectBreakingChange [

	| driver interval |
	self setUpDataForTesting.
	(model classNamed: #RBInlineSub) compile: 'foo ^ 99' classified: #( #accessing ).
	interval := self intervalForMessageSend: 'self foo' inMethod: #caller ofClass: (model classNamed: #RBInlineBase).

	driver := ReInlineMethodDriver new
		          scopes: { environment }
		          interval: interval
		          selector: #caller
		          class: (model classNamed: #RBInlineBase).


	self setUpDriver: driver.
	driver runRefactoring.

	self assert: driver refactoring failedBreakingChangePreconditions size equals: 1.
	self assert: (driver refactoring failedBreakingChangePreconditions first isKindOf: ReSubclassesDirectlyDefinesCondition)
]
