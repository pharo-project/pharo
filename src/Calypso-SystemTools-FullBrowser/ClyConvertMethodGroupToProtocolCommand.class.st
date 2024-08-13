"
I am a command to convert given method groups to a protocol.
I perform kind of ""Move to protocol"" operation for all contained methods
 
Internal Representation and Key Implementation Points.

    Instance Variables
	protocol:		<Symbol>
"
Class {
	#name : 'ClyConvertMethodGroupToProtocolCommand',
	#superclass : 'ClyMethodGroupCommand',
	#instVars : [
		'protocol'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-MethodGroups',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-MethodGroups'
}

{ #category : 'testing' }
ClyConvertMethodGroupToProtocolCommand class >> canBeExecutedInContext: aToolContext [

	^ aToolContext isExtensionMethodGroupSelected
]

{ #category : 'activation' }
ClyConvertMethodGroupToProtocolCommand class >> fullBrowserContextMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemFor: ClyMethodGroup asCalypsoItemContext
]

{ #category : 'execution' }
ClyConvertMethodGroupToProtocolCommand >> applyResultInContext: aToolContext [

	super applyResultInContext: aToolContext.
	aToolContext showProtocol: protocol
]

{ #category : 'accessing' }
ClyConvertMethodGroupToProtocolCommand >> defaultMenuItemName [

	^ 'Convert to protocol'
]

{ #category : 'execution' }
ClyConvertMethodGroupToProtocolCommand >> execute [

	methodGroups do: [ :methodGroup | methodGroup categorizeMethodsIn: protocol ]
]

{ #category : 'execution' }
ClyConvertMethodGroupToProtocolCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.

	protocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter | presenter concernedClass: aToolContext lastSelectedClass ]
]

{ #category : 'accessing' }
ClyConvertMethodGroupToProtocolCommand >> protocol [

	^ protocol
]

{ #category : 'accessing' }
ClyConvertMethodGroupToProtocolCommand >> protocol: anObject [

	protocol := anObject
]
