"
SDL2 library structure representing a joystick device added event data.
"
Class {
	#name : 'SDL_JoyDeviceAddedEvent',
	#superclass : 'SDL_JoyDeviceEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_JoyDeviceAddedEvent class >> eventType [
	^ SDL_JOYDEVICEADDED
]

{ #category : 'visitor' }
SDL_JoyDeviceAddedEvent >> accept: aVisitor [
	^ aVisitor visitJoyDeviceAddedEvent: self
]
