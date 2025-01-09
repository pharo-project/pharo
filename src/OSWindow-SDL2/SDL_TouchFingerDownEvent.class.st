"
SDL2 library structure
"
Class {
	#name : 'SDL_TouchFingerDownEvent',
	#superclass : 'SDL_TouchFingerEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_TouchFingerDownEvent class >> eventType [
	^ SDL_FINGERDOWN
]

{ #category : 'visitor' }
SDL_TouchFingerDownEvent >> accept: aVisitor [
	^ aVisitor visitFingerDownEvent: self
]
