"
SDL2 library structure
"
Class {
	#name : #'SDL_TouchFingerUpEvent',
	#superclass : #'SDL_TouchFingerEvent',
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'event type' }
SDL_TouchFingerUpEvent class >> eventType [
	^SDL_FINGERUP
]

{ #category : #visitor }
SDL_TouchFingerUpEvent >> accept: aVisitor [
	^ aVisitor visitFingerUpEvent: self
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> dx [
	"This method was automatically generated"
	^handle floatAt: OFFSET_DX
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> dx: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_DX put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> dy [
	"This method was automatically generated"
	^handle floatAt: OFFSET_DY
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> dy: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_DY put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> fingerId [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_FINGERID
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> fingerId: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_FINGERID put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> pressure [
	"This method was automatically generated"
	^handle floatAt: OFFSET_PRESSURE
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> pressure: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_PRESSURE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> touchId [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_TOUCHID
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> touchId: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_TOUCHID put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> x [
	"This method was automatically generated"
	^handle floatAt: OFFSET_X
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> x: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_X put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> y [
	"This method was automatically generated"
	^handle floatAt: OFFSET_Y
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerUpEvent >> y: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_Y put: anObject
]
