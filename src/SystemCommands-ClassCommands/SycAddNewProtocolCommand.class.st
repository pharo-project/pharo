"
I am a command to add new protocol top the class 

Internal Representation and Key Implementation Points.

    Instance Variables
	protocol:		<String>
	targetClass:		<Class>
"
Class {
	#name : 'SycAddNewProtocolCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'targetClass',
		'protocol'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycAddNewProtocolCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showProtocol: protocol
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> defaultMenuIconName [

	^ #add
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> defaultMenuItemName [

	^ 'New protocol'
]

{ #category : 'execution' }
SycAddNewProtocolCommand >> execute [

	targetClass addProtocol: protocol
]

{ #category : 'execution' }
SycAddNewProtocolCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	protocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter | presenter concernedClass: aToolContext lastSelectedClass ].

	targetClass := self requestClassInContext: aToolContext.
	targetClass := aToolContext currentMetaLevelOf: targetClass
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

	| classes |
	classes := aToolContext selectedClasses.
	classes size = 1 ifTrue: [ ^ classes first ].

	^ aToolContext requestSingleClass: 'In what class you want a protocol?' from: classes
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> targetClass [
	^ targetClass
]

{ #category : 'accessing' }
SycAddNewProtocolCommand >> targetClass: anObject [
	targetClass := anObject
]
