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

{ #category : 'testing' }
SycMockClassContext >> isClassSelected [
	^ self selectedObjects isNotEmpty
]

{ #category : 'accessing' }
SycMockClassContext >> lastSelectedClass [

	^ self selectedObjects
		  ifEmpty: [ nil ]
		  ifNotEmpty: [ :objects | objects first ]
]

{ #category : 'accessing' }
SycMockClassContext >> selectedClasses [
	^ self selectedObjects
]

{ #category : 'accessing' }
SycMockClassContext >> selectedObjects [
	^ selectedObjects ifNil: [ #() ]
]

{ #category : 'accessing' }
SycMockClassContext >> selectedObjects: aCollection [
	selectedObjects := aCollection
]
