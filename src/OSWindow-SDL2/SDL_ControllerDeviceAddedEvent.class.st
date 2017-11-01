"
SDL2 library structure representing a controller device added event data.
"
Class {
	#name : #SDL_ControllerDeviceAddedEvent,
	#superclass : #SDL_ControllerDeviceEvent,
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'event type' }
SDL_ControllerDeviceAddedEvent class >> eventType [
	^ SDL_CONTROLLERDEVICEADDED
]

{ #category : #visitor }
SDL_ControllerDeviceAddedEvent >> accept: aVisitor [
	^ aVisitor visitControllerDeviceAddedEvent: self
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceAddedEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceAddedEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceAddedEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceAddedEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceAddedEvent >> which [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_WHICH
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceAddedEvent >> which: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_WHICH put: anObject
]
