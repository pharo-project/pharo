"
SDL2 library structure
"
Class {
	#name : #SDL_TouchFingerMotionEvent,
	#superclass : #SDL_TouchFingerEvent,
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'event type' }
SDL_TouchFingerMotionEvent class >> eventType [
	^ SDL_FINGERMOTION
]

{ #category : #visitor }
SDL_TouchFingerMotionEvent >> accept: aVisitor [
	^ aVisitor visitFingerMotionEvent: self

]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> dx [
	"This method was automatically generated"
	^handle floatAt: OFFSET_DX
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> dx: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_DX put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> dy [
	"This method was automatically generated"
	^handle floatAt: OFFSET_DY
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> dy: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_DY put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> fingerId [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_FINGERID
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> fingerId: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_FINGERID put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> pressure [
	"This method was automatically generated"
	^handle floatAt: OFFSET_PRESSURE
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> pressure: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_PRESSURE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> touchId [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_TOUCHID
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> touchId: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_TOUCHID put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> x [
	"This method was automatically generated"
	^handle floatAt: OFFSET_X
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> x: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_X put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> y [
	"This method was automatically generated"
	^handle floatAt: OFFSET_Y
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerMotionEvent >> y: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_Y put: anObject
]
