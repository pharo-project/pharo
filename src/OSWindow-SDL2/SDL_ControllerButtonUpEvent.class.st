"
SDL2 library structure representing a game controller button up event data.
"
Class {
	#name : 'SDL_ControllerButtonUpEvent',
	#superclass : 'SDL_ControllerButtonEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_ControllerButtonUpEvent class >> eventType [
	^ SDL_CONTROLLERBUTTONUP
]

{ #category : 'visitor' }
SDL_ControllerButtonUpEvent >> accept: aVisitor [
	^ aVisitor visitControllerButtonUpEvent: self
]
