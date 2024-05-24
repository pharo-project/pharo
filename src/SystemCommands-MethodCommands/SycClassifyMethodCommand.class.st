"
I am a command to classify given methods with a given protocol.

Internal Representation and Key Implementation Points.

    Instance Variables
	protocol:		<Symbol>
"
Class {
	#name : 'SycClassifyMethodCommand',
	#superclass : 'SycMethodCommand',
	#instVars : [
		'protocol'
	],
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycClassifyMethodCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showProtocol: protocol
]

{ #category : 'accessing' }
SycClassifyMethodCommand >> defaultMenuIconName [

	^ #glamorousBrowse
]

{ #category : 'accessing' }
SycClassifyMethodCommand >> defaultMenuItemName [

	^ 'Classify methods'
]

{ #category : 'execution' }
SycClassifyMethodCommand >> execute [

	methods do: [ :aMethod | aMethod protocol: protocol ]
]

{ #category : 'execution' }
SycClassifyMethodCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	protocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter | presenter concernedClass: aToolContext lastSelectedClass ]
]

{ #category : 'accessing' }
SycClassifyMethodCommand >> protocol [
	^ protocol
]

{ #category : 'accessing' }
SycClassifyMethodCommand >> protocol: protocolName [
	protocol := protocolName
]
