"
A morph that acts EXCLUSIVELY as a listener of mouse events. 
The purpose is to listen to mouse events and send them back to a `SpPopoverMorph`.

See `SpPopoverMorph>>#registerToDeleteEvents`.
"
Class {
	#name : #SpPopoverDeleteListenerMorph,
	#superclass : #Morph,
	#category : #'Spec2-Adapters-Morphic-Support'
}

{ #category : #'events-processing' }
SpPopoverDeleteListenerMorph >> handleMouseDown: evt [

	^ self eventHandler ifNotNil: [:handler | handler mouseDown: evt fromMorph: self ]
]

{ #category : #'events-processing' }
SpPopoverDeleteListenerMorph >> handleMouseMove: evt [

	^ self eventHandler ifNotNil: [:handler | handler mouseMove: evt fromMorph: self ]
]

{ #category : #'events-processing' }
SpPopoverDeleteListenerMorph >> handleMouseUp: evt [

	^ self eventHandler ifNotNil: [:handler | handler mouseUp: evt fromMorph: self ]
]
