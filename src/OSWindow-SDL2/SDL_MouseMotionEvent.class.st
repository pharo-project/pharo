"
SDL2 library structure
"
Class {
	#name : #SDL_MouseMotionEvent,
	#superclass : #SDL2MappedEvent,
	#classVars : [
		'OFFSET_STATE',
		'OFFSET_TIMESTAMP',
		'OFFSET_TYPE',
		'OFFSET_WHICH',
		'OFFSET_WINDOWID',
		'OFFSET_X',
		'OFFSET_XREL',
		'OFFSET_Y',
		'OFFSET_YREL'
	],
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'event type' }
SDL_MouseMotionEvent class >> eventType [
	^ SDL_MOUSEMOTION
]

{ #category : #'fields description' }
SDL_MouseMotionEvent class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID; 
    Uint32 which;
    Uint32 state;
    Sint32 x;
    Sint32 y;
    Sint32 xrel;
    Sint32 yrel;
 	)
]

{ #category : #visitor }
SDL_MouseMotionEvent >> accept: aVisitor [
	^ aVisitor visitMouseMotionEvent: self
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> state [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_STATE
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> state: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_STATE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> which [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_WHICH
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> which: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_WHICH put: anObject
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> windowID [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_WINDOWID
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> windowID: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_WINDOWID put: anObject
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> x [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_X
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> x: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_X put: anObject
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> xrel [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_XREL
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> xrel: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_XREL put: anObject
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> y [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_Y
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> y: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_Y put: anObject
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> yrel [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_YREL
]

{ #category : #'accessing structure variables' }
SDL_MouseMotionEvent >> yrel: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_YREL put: anObject
]
