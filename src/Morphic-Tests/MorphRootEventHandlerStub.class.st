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
MorphRootEventHandlerStub >> handleContentsMenu: anEvent for: aWorld [

	handled add: #contentsMenu -> aWorld.
	^ handles
]

{ #category : 'event handling' }
MorphRootEventHandlerStub >> handleDragThrough: anEvent for: aWorld [

	handled add: #dragThrough -> aWorld.
	^ handles
]

{ #category : 'event handling' }
MorphRootEventHandlerStub >> handleEvent: anEvent in: aWorld [

	handled add: anEvent type -> aWorld.
	^ handles
]

{ #category : 'event handling' }
MorphRootEventHandlerStub >> handleNavigationKey: aKeyboardEvent for: aWorld [

	handled add: #navigationKey -> aWorld.
	^ handles
]

{ #category : 'event handling' }
MorphRootEventHandlerStub >> handleWindowClose: anEvent for: aWorld [

	handled add: #windowClose -> aWorld.
	^ handles
]

{ #category : 'event handling' }
MorphRootEventHandlerStub >> handleWorldMenu: anEvent for: aWorld [

	handled add: #worldMenu -> aWorld.
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
