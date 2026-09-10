"
A mock of context for testing commands
"
Class {
	#name : 'SycMockClassContext',
	#superclass : 'Object',
	#instVars : [
		'selectedObjects'
	],
	#category : 'SystemCommands-ClassCommands-Tests-Tests',
	#package : 'SystemCommands-ClassCommands-Tests',
	#tag : 'Tests'
}

{ #category : 'accessing' }
SycMockClassContext >> selectedObjects [
	^ selectedObjects ifNil: [ #() ]
]

{ #category : 'accessing' }
SycMockClassContext >> selectedObjects: aCollection [
	selectedObjects := aCollection
]
