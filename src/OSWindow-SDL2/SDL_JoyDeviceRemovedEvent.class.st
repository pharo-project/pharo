"
SDL2 library structure representing a joystick device removed event data.
"
Class {
	#name : 'SDL_JoyDeviceRemovedEvent',
	#superclass : 'SDL_JoyDeviceEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_JoyDeviceRemovedEvent class >> eventType [
	^ SDL_JOYDEVICEREMOVED
]

{ #category : 'visitor' }
SDL_JoyDeviceRemovedEvent >> accept: aVisitor [
	^ aVisitor visitJoyDeviceRemovedEvent: self
]
