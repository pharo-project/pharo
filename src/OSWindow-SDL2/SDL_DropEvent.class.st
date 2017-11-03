"
SDL2 library structure representing a dropped file into the window.
"
Class {
	#name : #SDL_DropEvent,
	#superclass : #SDL2MappedEvent,
	#classVars : [
		'OFFSET_FILE',
		'OFFSET_TIMESTAMP',
		'OFFSET_TYPE'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'event type' }
SDL_DropEvent class >> eventType [
	^ SDL_DROPFILE
]

{ #category : #'fields description' }
SDL_DropEvent class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    char* file;
 	)
]

{ #category : #visitor }
SDL_DropEvent >> accept: aVisitor [
	^ aVisitor visitDropEvent: self
]

{ #category : #'accessing structure variables' }
SDL_DropEvent >> file [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_FILE) type: ExternalType char asPointerType
]

{ #category : #'accessing structure variables' }
SDL_DropEvent >> file: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_FILE put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_DropEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_DropEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_DropEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_DropEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]
