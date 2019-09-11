"
I am a command to add new method tag (protocol) top the class 

Internal Representation and Key Implementation Points.

    Instance Variables
	tagName:		<String>
	targetClass:		<Class>
"
Class {
	#name : #SycAddNewMethodTagCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'targetClass',
		'tagName'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #execution }
SycAddNewMethodTagCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showMethodTag: tagName
]

{ #category : #accessing }
SycAddNewMethodTagCommand >> defaultMenuIconName [
	^#add
]

{ #category : #accessing }
SycAddNewMethodTagCommand >> defaultMenuItemName [
	^'New protocol'
]

{ #category : #execution }
SycAddNewMethodTagCommand >> execute [
	
	targetClass addMethodTag: tagName asSymbol
]

{ #category : #execution }
SycAddNewMethodTagCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	tagName := aToolContext requestSingleMethodTag: 'New protocol name'.
	
	targetClass := self requestClassInContext: aToolContext.
	targetClass := aToolContext currentMetaLevelOf: targetClass
]

{ #category : #execution }
SycAddNewMethodTagCommand >> requestClassInContext: aToolContext [
	| classes |
	classes := aToolContext selectedClasses.
	classes size = 1 ifTrue: [ ^classes first ].
	
	^aToolContext  
		requestSingleClass: 'In what class you want a protocol?' 
		from: classes
]

{ #category : #accessing }
SycAddNewMethodTagCommand >> tagName [
	^ tagName
]

{ #category : #accessing }
SycAddNewMethodTagCommand >> tagName: anObject [
	tagName := anObject
]

{ #category : #accessing }
SycAddNewMethodTagCommand >> targetClass [
	^ targetClass
]

{ #category : #accessing }
SycAddNewMethodTagCommand >> targetClass: anObject [
	targetClass := anObject
]
