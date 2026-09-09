"
SDL2 library structure representing a controller device added event data.
"
Class {
	#name : 'SDL_ControllerDeviceAddedEvent',
	#superclass : 'SDL_ControllerDeviceEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_ControllerDeviceAddedEvent class >> eventType [
	^ SDL_CONTROLLERDEVICEADDED
]

{ #category : 'visitor' }
SDL_ControllerDeviceAddedEvent >> accept: aVisitor [
	^ aVisitor visitControllerDeviceAddedEvent: self
]
