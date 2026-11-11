"
I am a mouse activation which runs a command on a mouse double click.
"
Class {
	#name : 'ClyDoubleClickActivation',
	#superclass : 'ClyMouseCommandActivation',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'command registry' }
ClyDoubleClickActivation class >> interactionName [

	^ #doubleClick
]

{ #category : 'testing' }
ClyDoubleClickActivation class >> isAbstract [

	^ self == ClyDoubleClickActivation
]

{ #category : 'morphs setup' }
ClyDoubleClickActivation class >> morphicEventType [
	^#doubleClick
]
