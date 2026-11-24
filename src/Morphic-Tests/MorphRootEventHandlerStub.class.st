"
I record the meta gestures I am asked about, as gesture name -> morph, and answer whether I handle them (yes, unless told to decline).
"
Class {
	#name : 'MorphRootEventHandlerStub',
	#superclass : 'MorphRootEventHandler',
	#instVars : [
		'handled',
		'handles'
	],
	#category : 'Morphic-Tests-Events',
	#package : 'Morphic-Tests',
	#tag : 'Events'
}

{ #category : 'accessing' }
MorphRootEventHandlerStub >> decline [
	"Answer that I did not handle the gestures I am asked about"

	handles := false
]

{ #category : 'event handling' }
MorphRootEventHandlerStub >> handleEvent: anEvent in: aWorld [

	handled add: anEvent type -> aWorld.
	^ handles
]

{ #category : 'accessing' }
MorphRootEventHandlerStub >> handled [

	^ handled
]

{ #category : 'initialization' }
MorphRootEventHandlerStub >> initialize [

	super initialize.
	handled := OrderedCollection new.
	handles := true
]
