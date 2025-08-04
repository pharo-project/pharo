"
SDL2 library structure
"
Class {
	#name : 'SDL_TouchFingerMotionEvent',
	#superclass : 'SDL_TouchFingerEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_TouchFingerMotionEvent class >> eventType [
	^ SDL_FINGERMOTION
]

{ #category : 'visitor' }
SDL_TouchFingerMotionEvent >> accept: aVisitor [
	^ aVisitor visitFingerMotionEvent: self
]
