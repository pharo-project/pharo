"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'MCMockASubclass',
	#superclass : 'MCMockClassA',
	#instVars : [
		'x'
	],
	#classVars : [
		'Y'
	],
	#category : 'MonticelloMocks',
	#package : 'MonticelloMocks'
}

{ #category : 'class initialization' }
MCMockASubclass class >> initialize [

	InitializationOrder := InitializationOrder
		ifNil: [ -100 ] "let the test fail"
		ifNotNil: [ InitializationOrder + 1 ]
]

{ #category : 'as yet unclassified' }
MCMockASubclass >> variables [
	^ x + Y + MCMockClassA
]

{ #category : 'as yet unclassified' }
MCMockASubclass >> variables2 [
	^ ivar + CVar
]
