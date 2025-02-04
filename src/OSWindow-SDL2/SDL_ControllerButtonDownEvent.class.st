"
SDL2 library structure representing a game controller button down event data.
"
Class {
	#name : 'SDL_ControllerButtonDownEvent',
	#superclass : 'SDL_ControllerButtonEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_ControllerButtonDownEvent class >> eventType [
	^ SDL_CONTROLLERBUTTONDOWN
]

{ #category : 'visitor' }
SDL_ControllerButtonDownEvent >> accept: aVisitor [
	^ aVisitor visitControllerButtonDownEvent: self
]
