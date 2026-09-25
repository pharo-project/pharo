"
A mock of context for testing commands
"
Class {
	#name : 'SycMockMethodContext',
	#superclass : 'Object',
	#instVars : [
		'selectedMethods'
	],
	#category : 'SystemCommands-MethodCommands-Tests-TestData',
	#package : 'SystemCommands-MethodCommands-Tests',
	#tag : 'TestData'
}

{ #category : 'accessing' }
SycMockMethodContext >> selectedMethods [
	^ selectedMethods ifNil: [ #() ]
]

{ #category : 'accessing' }
SycMockMethodContext >> selectedMethods: aCollection [
	selectedMethods := aCollection
]
