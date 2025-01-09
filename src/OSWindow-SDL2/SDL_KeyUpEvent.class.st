"
SDL2 library structure
"
Class {
	#name : 'SDL_KeyUpEvent',
	#superclass : 'SDL_KeyboardEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_KeyUpEvent class >> eventType [
	^ SDL_KEYUP
]

{ #category : 'visitor' }
SDL_KeyUpEvent >> accept: aVisitor [
	^ aVisitor visitKeyUpEvent: self
]

{ #category : 'testing' }
SDL_KeyUpEvent >> isKeyUpEvent [

	^ true
]
