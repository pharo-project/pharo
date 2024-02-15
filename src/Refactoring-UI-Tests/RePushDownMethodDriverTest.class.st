Class {
	#name : 'RePushDownMethodDriverTest',
	#superclass : 'ReDriverTest',
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'tests' }
RePushDownMethodDriverTest >> testPushDownMethodDriverWhenHappyPathExpectChanges [

	| rbclasses driver method model |
	rbclasses := RBClassEnvironment classes: MyClassA withAllSubclasses.
	model := RBNamespace onEnvironment: rbclasses.
	method := MyClassA methodNamed: #methodForPushDown.
	
	driver := RePushDownMethodDriver new
					model: model scopes: { } pushDownMethods: { method }.

	self setUpDriver: driver.
	
	driver runRefactoring.
	self assert: driver refactoring model changes changes size equals: 4
]
