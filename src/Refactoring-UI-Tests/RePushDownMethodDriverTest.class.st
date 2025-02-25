Class {
	#name : 'RePushDownMethodDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'model',
		'environment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'running' }
RePushDownMethodDriverTest >> createHierarchySubclassesReferToSelector [

	environment := RBPackageEnvironment packageName: 'Refactoring-DataForTesting'.
	model := RBNamespace onEnvironment: environment.
	model
		defineClass: [ :aBuilder |
			aBuilder
				superclass: Object;
				name: #Superclass;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #Superclass;
				name: #Foo1;
				package: #'Refactory-Test data' ];
		defineClass: [ :aBuilder |
			aBuilder
				superclassName: #Superclass;
				name: #Foo2;
				package: #'Refactory-Test data' ].
	(model classNamed: #Superclass) compile: 'yourself ^1' classified: #( #accessing ).
	(model classNamed: #Foo1) compile: 'method1 ^super yourself' classified: #( #accessing ).
]

{ #category : 'initialization' }
RePushDownMethodDriverTest >> setUpDriver: driver [

	super setUpDriver: driver.
	
	driver methodsSelectionPresenterClass: StMethodsSelectionPresenterMock
]

{ #category : 'tests' }
RePushDownMethodDriverTest >> testPushDownMethodDriverWhenAbstractClassExpectBreakingChanges [

	| driver method |
	environment := RBClassEnvironment classes: Array withAllSubclasses.
	model := RBNamespace onEnvironment: environment.
	method := Array methodNamed: #isArray.
	
	driver := RePushDownMethodDriver new
					model: model scopes: { environment } pushDownMethods: { method }.

	self setUpDriver: driver.
	
	driver runRefactoring.
	self assert: driver refactoring model changes changes size equals: 0.
	self assert: driver refactoring failedBreakingChangePreconditions size equals: 1
]

{ #category : 'tests' }
RePushDownMethodDriverTest >> testPushDownMethodDriverWhenHappyPathExpectChanges [

	| driver method |
	environment := RBClassEnvironment classes: MyClassA withAllSubclasses.
	model := RBNamespace onEnvironment: environment.
	method := MyClassA methodNamed: #methodForPushDown.
	
	driver := RePushDownMethodDriver new
					model: model scopes: { environment } pushDownMethods: { method }.

	self setUpDriver: driver.
	
	driver runRefactoring.
	self assert: driver refactoring model changes changes size equals: 4
]

{ #category : 'tests' }
RePushDownMethodDriverTest >> testPushDownMethodDriverWhenSubclassesReferToSelectorExpectBreakingChanges [

	| driver method class |
	
	self createHierarchySubclassesReferToSelector.
	class := model classNamed: 'Superclass'.
	method := class methodFor: #yourself.
	
	driver := RePushDownMethodDriver new
					model: model scopes: { environment } pushDownMethods: { method }.

	self setUpDriver: driver.
	
	driver runRefactoring.
	self assert: driver refactoring model changes changes size equals: 5.
	self assert: driver refactoring failedBreakingChangePreconditions size equals: 1
]
