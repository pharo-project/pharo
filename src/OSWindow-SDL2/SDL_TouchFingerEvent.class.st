"
SDL2 library structure
"
Class {
	#name : #SDL_TouchFingerEvent,
	#superclass : #SDL2MappedEvent,
	#classVars : [
		'OFFSET_DX',
		'OFFSET_DY',
		'OFFSET_FINGERID',
		'OFFSET_PRESSURE',
		'OFFSET_TIMESTAMP',
		'OFFSET_TOUCHID',
		'OFFSET_TYPE',
		'OFFSET_X',
		'OFFSET_Y'
	],
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'fields description' }
SDL_TouchFingerEvent class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Sint64 touchId;
    Sint64 fingerId;
    float x;
    float y;
 	 float dx;
    float dy;
	 float pressure;
 	)	
	
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> dx [
	"This method was automatically generated"
	^handle floatAt: OFFSET_DX
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> dx: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_DX put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> dy [
	"This method was automatically generated"
	^handle floatAt: OFFSET_DY
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> dy: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_DY put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> fingerId [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_FINGERID
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> fingerId: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_FINGERID put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> pressure [
	"This method was automatically generated"
	^handle floatAt: OFFSET_PRESSURE
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> pressure: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_PRESSURE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> touchId [
	"This method was automatically generated"
	^handle signedLongLongAt: OFFSET_TOUCHID
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> touchId: anObject [
	"This method was automatically generated"
	handle signedLongLongAt: OFFSET_TOUCHID put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> x [
	"This method was automatically generated"
	^handle floatAt: OFFSET_X
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> x: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_X put: anObject
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> y [
	"This method was automatically generated"
	^handle floatAt: OFFSET_Y
]

{ #category : #'accessing structure variables' }
SDL_TouchFingerEvent >> y: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_Y put: anObject
]
