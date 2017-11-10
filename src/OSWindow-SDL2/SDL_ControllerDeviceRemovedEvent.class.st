"
SDL2 library structure representing a controller device removed data.
"
Class {
	#name : #SDL_ControllerDeviceRemovedEvent,
	#superclass : #SDL_ControllerDeviceEvent,
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'event type' }
SDL_ControllerDeviceRemovedEvent class >> eventType [
	^ SDL_CONTROLLERDEVICEREMOVED
]

{ #category : #visitor }
SDL_ControllerDeviceRemovedEvent >> accept: aVisitor [
	^ aVisitor visitControllerDeviceRemovedEvent: self
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemovedEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemovedEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemovedEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemovedEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemovedEvent >> which [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_WHICH
]

{ #category : #'accessing structure variables' }
SDL_ControllerDeviceRemovedEvent >> which: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_WHICH put: anObject
]
