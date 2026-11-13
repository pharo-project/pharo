"
I am the command which opens the menu of the left bar of a text editor. When no text is selected, I first show the source node of the method, and then I open the menu.
"
Class {
	#name : 'ClyOpenTextLeftBarMenuCommand',
	#superclass : 'ClyOpenContextMenuCommand',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'annotations' }
ClyOpenTextLeftBarMenuCommand class >> contextClickActivation [
	<classAnnotation>

	^ ClyTextLeftBarClickActivation byYellowButtonFor: ClyToolContext
]

{ #category : 'execution' }
ClyOpenTextLeftBarMenuCommand >> activationStrategy [
	^ClyTextLeftBarMenuActivation
]

{ #category : 'execution' }
ClyOpenTextLeftBarMenuCommand >> execute [

	self context selectedTextInterval ifEmpty: [
		self context showSourceNode].

	super execute
]
