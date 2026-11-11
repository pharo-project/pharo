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

{ #category : 'execution' }
ClyConvertMethodGroupToProtocolCommand >> applyCommandResult [

	super applyCommandResult.
	self context showProtocol: protocol
]

{ #category : 'execution' }
ClyConvertMethodGroupToProtocolCommand >> executeRefactoring [

	methodGroups do: [ :methodGroup | methodGroup categorizeMethodsIn: protocol ]
]

{ #category : 'accessing' }
ClyConvertMethodGroupToProtocolCommand >> name [

	^ 'Convert to protocol'
]

{ #category : 'accessing' }
ClyConvertMethodGroupToProtocolCommand >> order [

	^ 1000
]

{ #category : 'execution' }
ClyConvertMethodGroupToProtocolCommand >> prepareFullExecution [

	super prepareFullExecution.
	protocol := StProtocolNameChooserPresenter requestProtocolNameConfiguring: [ :presenter | presenter concernedClass: self context lastSelectedClass ]
]

{ #category : 'accessing' }
ClyConvertMethodGroupToProtocolCommand >> protocol [

	^ protocol
]

{ #category : 'accessing' }
ClyConvertMethodGroupToProtocolCommand >> protocol: anObject [

	protocol := anObject
]
