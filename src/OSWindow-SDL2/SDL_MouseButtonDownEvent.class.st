"
SDL2 library structure
"
Class {
	#name : 'SDL_MouseButtonDownEvent',
	#superclass : 'SDL_MouseButtonEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_MouseButtonDownEvent class >> eventType [
	^ SDL_MOUSEBUTTONDOWN
]

{ #category : 'visitor' }
SDL_MouseButtonDownEvent >> accept: aVisitor [
	^ aVisitor visitMouseButtonDownEvent: self
]
