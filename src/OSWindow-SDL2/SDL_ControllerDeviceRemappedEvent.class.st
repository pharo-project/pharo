"
SDL2 library structure representing a controller device remapped event data.
"
Class {
	#name : #SDL_ControllerDeviceRemappedEvent,
	#superclass : #SDL_ControllerDeviceEvent,
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'event type' }
SDL_ControllerDeviceRemappedEvent class >> eventType [
	^ SDL_CONTROLLERDEVICEREMAPPED
]

{ #category : #visitor }
SDL_ControllerDeviceRemappedEvent >> accept: aVisitor [
	^ aVisitor visitControllerDeviceRemappedEvent: self
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemappedEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemappedEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemappedEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemappedEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemappedEvent >> which [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_WHICH
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemappedEvent >> which: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_WHICH put: anObject
]
