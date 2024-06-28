Class {
	#name : 'RePullUpMethodDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'model',
		'environment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'initialization' }
RePullUpMethodDriverTest >> setUpDriver: driver [

	super setUpDriver: driver.
	
	driver methodsSelectionPresenterClass: StSelectClassAndMethodsPresenterMock
]

{ #category : 'initialization' }
RePullUpMethodDriverTest >> testPullUpMethodDriverWhenHappyPathExpectChanges [

	| driver method |
	
	environment := RBClassEnvironment classes: MyClassAlpha withAllSubclasses.
	model := RBNamespace onEnvironment: environment.
	method := MyClassBeta methodNamed: #methodForPullUp.
	
	driver := RePullUpMethodDriver new
					model: model scopes: { environment } pullUpMethods: { method }.

	self setUpDriver: driver.
	
	driver runRefactoring.
	self assert: driver refactoring model changes changes size equals: 2
]

{ #category : 'initialization' }
RePullUpMethodDriverTest >> testPullUpMethodDriverWhenOverridesExpectBreakingChanges [

	| driver method |
	
	environment := RBClassEnvironment classes: MyClassAlpha withAllSubclasses.
	model := RBNamespace onEnvironment: environment.
	method := MyClassBeta methodNamed: #methodOverriden.
	
	driver := RePullUpMethodDriver new
					model: model scopes: { environment } pullUpMethods: { method }.

	self setUpDriver: driver.
	
	driver runRefactoring.
	self assert: driver refactoring model changes changes size equals: 0.
	self assert: driver refactoring failedApplicabilityPreconditions size equals: 1
]
