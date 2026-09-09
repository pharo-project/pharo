"
SDL2 library structure representing a controller device removed data.
"
Class {
	#name : 'SDL_ControllerDeviceRemovedEvent',
	#superclass : 'SDL_ControllerDeviceEvent',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_ControllerDeviceRemovedEvent class >> eventType [
	^ SDL_CONTROLLERDEVICEREMOVED
]

{ #category : 'visitor' }
SDL_ControllerDeviceRemovedEvent >> accept: aVisitor [
	^ aVisitor visitControllerDeviceRemovedEvent: self
]
