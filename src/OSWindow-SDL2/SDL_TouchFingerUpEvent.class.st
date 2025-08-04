"
SDL2 library structure
"
Class {
	#name : 'SDL_TouchFingerUpEvent',
	#superclass : 'SDL_TouchFingerEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_TouchFingerUpEvent class >> eventType [
	^SDL_FINGERUP
]

{ #category : 'visitor' }
SDL_TouchFingerUpEvent >> accept: aVisitor [
	^ aVisitor visitFingerUpEvent: self
]
