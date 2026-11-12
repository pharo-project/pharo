"
I am a click activation of the left bar of a text editor. I run a command from a click on a line of the left bar.
"
Class {
	#name : 'ClyTextLeftBarClickActivation',
	#superclass : 'ClyClickActivation',
	#category : 'Commander-Calypso-Compatibility',
	#package : 'Commander-Calypso-Compatibility'
}

{ #category : 'command registry' }
ClyTextLeftBarClickActivation class >> interactionName [

	^ #textLeftBarClick
]
