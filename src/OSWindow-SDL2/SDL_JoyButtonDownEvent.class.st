"
SDL2 library structure representing a joystick button down event data.
"
Class {
	#name : 'SDL_JoyButtonDownEvent',
	#superclass : 'SDL_JoyButtonEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_JoyButtonDownEvent class >> eventType [
	^ SDL_JOYBUTTONDOWN
]

{ #category : 'visitor' }
SDL_JoyButtonDownEvent >> accept: aVisitor [
	^ aVisitor visitJoyButtonDownEvent: self
]
