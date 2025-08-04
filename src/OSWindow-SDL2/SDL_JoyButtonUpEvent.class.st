"
SDL2 library structure representing a joystick button up event data.
"
Class {
	#name : 'SDL_JoyButtonUpEvent',
	#superclass : 'SDL_JoyButtonEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_JoyButtonUpEvent class >> eventType [
	^ SDL_JOYBUTTONUP
]

{ #category : 'visitor' }
SDL_JoyButtonUpEvent >> accept: aVisitor [
	^ aVisitor visitJoyButtonUpEvent: self
]
