"
I represent activation of commands using mouse click event
"
Class {
	#name : #CmdClickActivation,
	#superclass : #CmdMouseCommandActivation,
	#category : #'Commander-Activators-Mouse'
}

{ #category : #'morphs setup' }
CmdClickActivation class >> enableInMorph: aMorph withCommandsFrom: someObjectWithCommandContext [
	super enableInMorph: aMorph withCommandsFrom: someObjectWithCommandContext.
	
	"Following subscribtion is requried because morphic does not generate mouse click 
	when right mouse (yellow) was clicked. In that case mouse up is triggered"
	
	aMorph 
		on: #mouseUp
		send: #handleYellowMouseClickUsing:event:from: to: self withValue: someObjectWithCommandContext
]

{ #category : #'morphs setup' }
CmdClickActivation class >> handleYellowMouseClickUsing: someObjectWithCommandContext event: anEvent from: sourceMorph [

	anEvent yellowButtonChanged ifFalse: [ ^ self ].
	
	^self handleMouseUsing: someObjectWithCommandContext event: anEvent from: sourceMorph
]

{ #category : #'morphs setup' }
CmdClickActivation class >> morphicEventType [
	^#click
]
