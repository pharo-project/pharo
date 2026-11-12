"
I am a base class for commands which perform operations with collection of classes
 
Internal Representation and Key Implementation Points.

    Instance Variables
	classes:		<Colletion of<Class>>
"
Class {
	#name : 'SycClassCommand',
	#superclass : 'ClyCommand',
	#traits : 'TRefactoringCommandSupport',
	#classTraits : 'TRefactoringCommandSupport classTrait',
	#instVars : [
		'classes'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'instance creation' }
SycClassCommand class >> for: classes [
	^self new
		classes: classes
]

{ #category : 'testing' }
SycClassCommand >> canBeExecuted [

	^ self context isClassSelected
]

{ #category : 'accessing' }
SycClassCommand >> classes [
	^ classes
]

{ #category : 'accessing' }
SycClassCommand >> classes: anObject [
	classes := anObject
]

{ #category : 'accessing' }
SycClassCommand >> orderInContext: aToolContext [

	^ aToolContext isFullBrowserClassContext
		ifTrue: [ self fullBrowserOrder ]
		ifFalse: [ self order ]
]

{ #category : 'execution' }
SycClassCommand >> prepareFullExecution [

	classes := self context selectedClasses
]
