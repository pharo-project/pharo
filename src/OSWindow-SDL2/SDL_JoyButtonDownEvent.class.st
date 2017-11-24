"
SDL2 library structure representing a joystick button down event data.
"
Class {
	#name : #'SDL_JoyButtonDownEvent',
	#superclass : #'SDL_JoyButtonEvent',
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'event type' }
SDL_JoyButtonDownEvent class >> eventType [
	^ SDL_JOYBUTTONDOWN
]

{ #category : #visitor }
SDL_JoyButtonDownEvent >> accept: aVisitor [
	^ aVisitor visitJoyButtonDownEvent: self
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> button [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_BUTTON
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> button: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_BUTTON put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> padding1 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING1
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> padding1: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING1 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> padding2 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING2
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> padding2: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING2 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> state [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_STATE
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> state: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_STATE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> which [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_WHICH
]

{ #category : #'accessing structure variables' }
SDL_JoyButtonDownEvent >> which: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_WHICH put: anObject
]
