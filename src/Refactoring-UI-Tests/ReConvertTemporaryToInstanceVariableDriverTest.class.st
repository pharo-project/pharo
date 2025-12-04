Class {
	#name : 'ReConvertTemporaryToInstanceVariableDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
	],
	#category : 'Refactoring-UI-Tests-Driver',
	#package : 'Refactoring-UI-Tests',
	#tag : 'Driver'
}

{ #category : 'getter' }
ReConvertTemporaryToInstanceVariableDriverTest >> classToConvertTemporaryToInstanceVariable [

	^ ReClassToConvertTemporaryToInstanceVariable
]

{ #category : 'running' }
ReConvertTemporaryToInstanceVariableDriverTest >> setUp [ 

	super setUp.
	testingEnvironment := RBClassEnvironment classes: self classToConvertTemporaryToInstanceVariable withAllSubclasses 
]

{ #category : 'tests' }
ReConvertTemporaryToInstanceVariableDriverTest >> testConvertTempFailure [

	| driver |
	driver := ReConvertTemporaryToInstanceVariableDriver new.
	self setUpDriver: driver.
	driver class: self classToConvertTemporaryToInstanceVariable selector: #doAction2 variable: 'instVar'.

	driver runRefactoring.

	self assert: driver refactoring changes changes size equals: 0
]

{ #category : 'tests' }
ReConvertTemporaryToInstanceVariableDriverTest >> testConvertTempSuccessfully [

	| driver |
	driver := ReConvertTemporaryToInstanceVariableDriver new.
	self setUpDriver: driver.
	driver class: self classToConvertTemporaryToInstanceVariable selector: #doAction1 variable: 'temp'.

	driver runRefactoring.

	self assert: driver refactoring changes changes size equals: 2.
	
]
