"
SDL2 library structure representing a joystick device removed event data.
"
Class {
	#name : #SDL_JoyDeviceRemovedEvent,
	#superclass : #SDL_JoyDeviceEvent,
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'event type' }
SDL_JoyDeviceRemovedEvent class >> eventType [
	^ SDL_JOYDEVICEREMOVED
]

{ #category : #visitor }
SDL_JoyDeviceRemovedEvent >> accept: aVisitor [
	^ aVisitor visitJoyDeviceRemovedEvent: self
]

{ #category : #'accessing structure variables' }
SDL_JoyDeviceRemovedEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_JoyDeviceRemovedEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyDeviceRemovedEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_JoyDeviceRemovedEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyDeviceRemovedEvent >> which [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_WHICH
]

{ #category : #'accessing structure variables' }
SDL_JoyDeviceRemovedEvent >> which: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_WHICH put: anObject
]
