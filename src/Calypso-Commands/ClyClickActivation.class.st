"
I am a mouse activation which runs a command on a mouse click. I also listen to mouse up, because morphic does not generate a click when the right (yellow) button is pressed.
"
Class {
	#name : 'ClyClickActivation',
	#superclass : 'ClyMouseCommandActivation',
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'morphs setup' }
ClyClickActivation class >> enableInMorph: aMorph withCommandsFrom: someObjectWithCommandContext [
	super enableInMorph: aMorph withCommandsFrom: someObjectWithCommandContext.

	"Following subscribtion is requried because morphic does not generate mouse click
	when right mouse (yellow) was clicked. In that case mouse up is triggered"

	aMorph
		on: #mouseUp
		send: #handleYellowMouseClickUsing:event:from: to: self withValue: someObjectWithCommandContext
]

{ #category : 'morphs setup' }
ClyClickActivation class >> handleYellowMouseClickUsing: someObjectWithCommandContext event: anEvent from: sourceMorph [

	anEvent yellowButtonChanged ifFalse: [ ^ self ].

	^self handleMouseUsing: someObjectWithCommandContext event: anEvent from: sourceMorph
]

{ #category : 'morphs setup' }
ClyClickActivation class >> morphicEventType [
	^#click
]
