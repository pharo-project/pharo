"
I am a base class for commands which perform operations with single class
 
Internal Representation and Key Implementation Points.

    Instance Variables
	targetClass:		<Class>
"
Class {
	#name : 'SycSingleClassCommand',
	#superclass : 'ClyCommand',
	#instVars : [
		'targetClass'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'testing' }
SycSingleClassCommand >> canBeExecuted [

	^ super canBeExecuted and: [ self context isClassSelected ]
]

{ #category : 'accessing' }
SycSingleClassCommand >> orderInContext: aToolContext [

	^ aToolContext isFullBrowserClassContext
		ifTrue: [ self fullBrowserOrder ]
		ifFalse: [ self order ]
]

{ #category : 'preparation' }
SycSingleClassCommand >> prepareFullExecution [

	targetClass := self context lastSelectedClass
]

{ #category : 'accessing' }
SycSingleClassCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
SycSingleClassCommand >> targetClass: anObject [
	targetClass := anObject
]
