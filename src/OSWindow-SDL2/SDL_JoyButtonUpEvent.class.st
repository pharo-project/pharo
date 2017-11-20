"
SDL2 library structure representing a joystick button up event data.
"
Class {
	#name : #SDL_JoyButtonUpEvent,
	#superclass : #SDL_JoyButtonEvent,
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'event type' }
SDL_JoyButtonUpEvent class >> eventType [
	^ SDL_JOYBUTTONUP
]

{ #category : #visitor }
SDL_JoyButtonUpEvent >> accept: aVisitor [
	^ aVisitor visitJoyButtonUpEvent: self
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> button [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_BUTTON
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> button: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_BUTTON put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> padding1 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING1
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> padding1: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING1 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> padding2 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING2
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> padding2: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING2 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> state [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_STATE
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> state: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_STATE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> which [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_WHICH
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonUpEvent >> which: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_WHICH put: anObject
]
