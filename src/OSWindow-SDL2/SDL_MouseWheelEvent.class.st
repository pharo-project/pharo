"
SDL2 library structure
"
Class {
	#name : 'SDL_MouseWheelEvent',
	#superclass : 'SDL2MappedEvent',
	#classVars : [
		'OFFSET_DIRECTION',
		'OFFSET_PRECISEX',
		'OFFSET_PRECISEY',
		'OFFSET_TIMESTAMP',
		'OFFSET_TYPE',
		'OFFSET_WHICH',
		'OFFSET_WINDOWID',
		'OFFSET_X',
		'OFFSET_Y'
	],
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_MouseWheelEvent class >> eventType [
	^ SDL_MOUSEWHEEL
]

{ #category : 'fields description' }
SDL_MouseWheelEvent class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    Uint32 which;
    Sint32 x;
    Sint32 y;
    SDL_MouseWheelDirection direction;
    float preciseX;
    float preciseY;
 	)
]

{ #category : 'visitor' }
SDL_MouseWheelEvent >> accept: aVisitor [
	^ aVisitor visitMouseWheelEvent: self
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> direction [
	"This method was automatically generated"
	^SDL_MouseWheelDirection fromInteger: (handle unsignedLongAt: OFFSET_DIRECTION)
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> direction: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_DIRECTION put: anObject value
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> preciseX [
	"This method was automatically generated"
	^handle floatAt: OFFSET_PRECISEX
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> preciseX: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_PRECISEX put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> preciseY [
	"This method was automatically generated"
	^handle floatAt: OFFSET_PRECISEY
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> preciseY: anObject [
	"This method was automatically generated"
	handle floatAt: OFFSET_PRECISEY put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> which [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_WHICH
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> which: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_WHICH put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> windowID [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_WINDOWID
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> windowID: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_WINDOWID put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> x [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_X
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> x: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_X put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> y [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_Y
]

{ #category : 'accessing - structure variables' }
SDL_MouseWheelEvent >> y: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_Y put: anObject
]
