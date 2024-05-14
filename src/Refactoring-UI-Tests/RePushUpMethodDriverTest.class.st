Class {
	#name : 'RePushUpMethodDriverTest',
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
RePushUpMethodDriverTest >> setUpDriver: driver [

	super setUpDriver: driver.
	
	driver methodsSelectionPresenterClass: StSelectClassAndMethodsPresenterMock
]

{ #category : 'initialization' }
RePushUpMethodDriverTest >> testPushUpMethodDriverWhenHappyPathExpectChanges [

	| driver method |
	
	environment := RBClassEnvironment classes: MyClassAlpha withAllSubclasses.
	model := RBNamespace onEnvironment: environment.
	method := MyClassBeta methodNamed: #methodForPushUp.
	
	driver := RePushUpMethodDriver new
					model: model scopes: { environment } pushUpMethods: { method }.

	self setUpDriver: driver.
	
	driver runRefactoring.
	self assert: driver refactoring model changes changes size equals: 2
]

{ #category : 'initialization' }
RePushUpMethodDriverTest >> testPushUpMethodDriverWhenOverridesExpectBreakingChanges [

	| driver method |
	
	environment := RBClassEnvironment classes: MyClassAlpha withAllSubclasses.
	model := RBNamespace onEnvironment: environment.
	method := MyClassBeta methodNamed: #methodOverriden.
	
	driver := RePushUpMethodDriver new
					model: model scopes: { environment } pushUpMethods: { method }.

	self setUpDriver: driver.
	
	driver runRefactoring.
	self assert: driver refactoring model changes changes size equals: 0.
	self assert: driver refactoring failedBreakingChangePreconditions size equals: 1
]
