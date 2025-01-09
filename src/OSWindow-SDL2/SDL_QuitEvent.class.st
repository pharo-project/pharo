"
SDL2 library structure
"
Class {
	#name : 'SDL_QuitEvent',
	#superclass : 'SDL_CommonEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_QuitEvent class >> eventType [
	^ SDL_QUIT
]

{ #category : 'visitor' }
SDL_QuitEvent >> accept: aVisitor [
	^ aVisitor visitQuitEvent: self
]
