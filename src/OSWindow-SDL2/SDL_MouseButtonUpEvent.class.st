"
SDL2 library structure
"
Class {
	#name : 'SDL_MouseButtonUpEvent',
	#superclass : 'SDL_MouseButtonEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_MouseButtonUpEvent class >> eventType [
	^ SDL_MOUSEBUTTONUP
]

{ #category : 'visitor' }
SDL_MouseButtonUpEvent >> accept: aVisitor [
	^ aVisitor visitMouseButtonUpEvent: self
]
