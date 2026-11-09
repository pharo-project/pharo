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

{ #category : 'testing' }
SycClassCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassSelected
]

{ #category : 'instance creation' }
SycClassCommand class >> for: classes [
	^self new
		classes: classes
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
SycClassCommand >> description [
	"Answer a <String> describing the receiver, useful for instance in menu items pop-ups"

	^ self class comment.
]

{ #category : 'accessing' }
SycClassCommand >> orderInContext: aToolContext [

	^ aToolContext isFullBrowserClassContext
		ifTrue: [ self fullBrowserOrder ]
		ifFalse: [ self order ]
]

{ #category : 'preparation' }
SycClassCommand >> prepareFullExecution [

	classes := self context selectedClasses
]
