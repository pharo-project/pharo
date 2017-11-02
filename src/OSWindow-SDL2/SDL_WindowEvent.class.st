"
SDL2 library structure
"
Class {
	#name : #SDL_WindowEvent,
	#superclass : #SDL2MappedEvent,
	#classVars : [
		'OFFSET_DATA1',
		'OFFSET_DATA2',
		'OFFSET_EVENT',
		'OFFSET_PADDING1',
		'OFFSET_PADDING2',
		'OFFSET_PADDING3',
		'OFFSET_TIMESTAMP',
		'OFFSET_TYPE',
		'OFFSET_WINDOWID'
	],
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'event type' }
SDL_WindowEvent class >> eventType [
	^ SDL_WINDOWEVENT
]

{ #category : #'fields description' }
SDL_WindowEvent class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    Uint8 event;
    Uint8 padding1;
    Uint8 padding2;
    Uint8 padding3;
    Sint32 data1;
    Sint32 data2;
 	)
]

{ #category : #visitor }
SDL_WindowEvent >> accept: aVisitor [
	^ aVisitor visitWindowEvent: self
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> data1 [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_DATA1
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> data1: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_DATA1 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> data2 [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_DATA2
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> data2: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_DATA2 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> event [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_EVENT
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> event: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_EVENT put: anObject
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> padding1 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING1
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> padding1: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING1 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> padding2 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING2
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> padding2: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING2 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> padding3 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING3
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> padding3: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING3 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> windowID [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_WINDOWID
]

{ #category : #'accessing structure variables' }
SDL_WindowEvent >> windowID: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_WINDOWID put: anObject
]
