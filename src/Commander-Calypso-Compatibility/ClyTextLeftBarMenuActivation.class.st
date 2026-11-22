"
I am the activation strategy of the commands shown in the menu of the left bar of a text editor. The commands are attached to me as class annotations, with the context they apply to.
"
Class {
	#name : 'ClyTextLeftBarMenuActivation',
	#superclass : 'ClyMenuCommandActivationStrategy',
	#category : 'Commander-Calypso-Compatibility',
	#package : 'Commander-Calypso-Compatibility'
}

{ #category : 'command registry' }
ClyTextLeftBarMenuActivation class >> interactionName [

	^ #textLeftBarMenu
]
