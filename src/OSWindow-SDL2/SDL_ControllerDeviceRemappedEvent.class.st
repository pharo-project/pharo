"
SDL2 library structure representing a controller device remapped event data.
"
Class {
	#name : 'SDL_ControllerDeviceRemappedEvent',
	#superclass : 'SDL_ControllerDeviceEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_ControllerDeviceRemappedEvent class >> eventType [
	^ SDL_CONTROLLERDEVICEREMAPPED
]

{ #category : 'visitor' }
SDL_ControllerDeviceRemappedEvent >> accept: aVisitor [
	^ aVisitor visitControllerDeviceRemappedEvent: self
]
