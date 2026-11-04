"
I am a command to add new protocol top the class 

Internal Representation and Key Implementation Points.

    Instance Variables
	protocol:		<String>
	targetClass:		<Class>
"
Class {
	#name : 'SycAddNewProtocolCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'targetClass',
		'protocol'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycAddNewProtocolCommand >> applyCommandResult [
	super applyCommandResult.
	self context showProtocol: protocol
]

{ #category : 'execution' }
SycAddNewProtocolCommand >> executeRefactoring [

	targetClass addProtocol: protocol
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> iconName [

	^ #add
]

{ #category : 'testing' }
SycAddNewProtocolCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserMethodGroupContext
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> name [

	^ 'New protocol'
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> order [

	^ 2
]

{ #category : 'execution' }
SycAddNewProtocolCommand >> prepareFullExecution [

	super prepareFullExecution.
	protocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter | presenter concernedClass: self context lastSelectedClass ].
	targetClass := self requestClassInContext: self context.
	targetClass := self context currentMetaLevelOf: targetClass
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> protocol [
	^ protocol
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> protocol: protocolName [
	protocol := protocolName
]

{ #category : 'execution' }
SycAddNewProtocolCommand >> requestClassInContext: aToolContext [

	| collectionOfClasses |
	collectionOfClasses := aToolContext selectedClasses.
	collectionOfClasses size = 1 ifTrue: [ ^ collectionOfClasses first ].

	^ aToolContext requestSingleClass: 'In what class you want a protocol?' from: collectionOfClasses
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> targetClass: anObject [
	targetClass := anObject
]
