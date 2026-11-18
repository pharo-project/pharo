"
I record the meta gestures I am asked to handle, as gesture name -> morph.
"
Class {
	#name : 'MorphRootEventHandlerStub',
	#superclass : 'MorphRootEventHandler',
	#instVars : [
		'handled'
	],
	#category : 'Morphic-Tests-Events',
	#package : 'Morphic-Tests',
	#tag : 'Events'
}

{ #category : 'event handling' }
MorphRootEventHandlerStub >> handleMetaMenuGesture: anEvent for: aMorph [

	handled add: #metaMenuGesture -> aMorph
]

{ #category : 'event handling' }
MorphRootEventHandlerStub >> handleSpecialGesture: anEvent for: aMorph [

	handled add: #specialGesture -> aMorph
]

{ #category : 'event handling' }
MorphRootEventHandlerStub >> handleYellowButtonMenu: shiftState for: aMorph [

	handled add: #yellowButtonMenu -> aMorph.
	^ true
]

{ #category : 'accessing' }
MorphRootEventHandlerStub >> handled [

	^ handled
]

{ #category : 'initialization' }
MorphRootEventHandlerStub >> initialize [

	super initialize.
	handled := OrderedCollection new
]
