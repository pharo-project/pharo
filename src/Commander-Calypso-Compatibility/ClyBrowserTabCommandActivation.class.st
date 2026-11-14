"
I am command activation strategy to access commands as tab manager icons. 
TabManagerMorph provides special place in tab panel (top right) for extended iconic buttons
"
Class {
	#name : 'ClyBrowserTabCommandActivation',
	#superclass : 'ClyMenuCommandActivationStrategy',
	#category : 'Commander-Calypso-Compatibility',
	#package : 'Commander-Calypso-Compatibility'
}

{ #category : 'command registry' }
ClyBrowserTabCommandActivation class >> interactionName [

	^ #tabActions
]
