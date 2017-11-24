"
SDL2 library structure
"
Class {
	#name : #'SDL_TouchFingerDownEvent',
	#superclass : #'SDL_TouchFingerEvent',
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'event type' }
SDL_TouchFingerDownEvent class >> eventType [
	^ SDL_FINGERDOWN
]

{ #category : #visitor }
SDL_TouchFingerDownEvent >> accept: aVisitor [
	^ aVisitor visitFingerDownEvent: self
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> dx [
	"This method was automatically generated"
	^handle floatAt: OFFSET_DX
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> dx: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_DX put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> dy [
	"This method was automatically generated"
	^handle floatAt: OFFSET_DY
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> dy: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_DY put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> fingerId [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_FINGERID
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> fingerId: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_FINGERID put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> pressure [
	"This method was automatically generated"
	^handle floatAt: OFFSET_PRESSURE
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> pressure: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_PRESSURE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> touchId [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_TOUCHID
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> touchId: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_TOUCHID put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> x [
	"This method was automatically generated"
	^handle floatAt: OFFSET_X
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> x: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_X put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> y [
	"This method was automatically generated"
	^handle floatAt: OFFSET_Y
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerDownEvent >> y: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_Y put: anObject
]
