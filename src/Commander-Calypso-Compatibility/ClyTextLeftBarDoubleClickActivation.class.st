"
I am a double click activation of the left bar of a text editor. I run a command from a double click on a line of the left bar.
"
Class {
	#name : 'ClyTextLeftBarDoubleClickActivation',
	#superclass : 'ClyDoubleClickActivation',
	#category : 'Commander-Calypso-Compatibility',
	#package : 'Commander-Calypso-Compatibility'
}

{ #category : 'command registry' }
ClyTextLeftBarDoubleClickActivation class >> interactionName [

	^ #textLeftBarDoubleClick
]
