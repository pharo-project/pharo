"
SDL2 library structure representing a game controller button event data.
"
Class {
	#name : #'SDL_ControllerButtonEvent',
	#superclass : #SDL2MappedEvent,
	#classVars : [
		'OFFSET_BUTTON',
		'OFFSET_PADDING1',
		'OFFSET_PADDING2',
		'OFFSET_STATE',
		'OFFSET_TIMESTAMP',
		'OFFSET_TYPE',
		'OFFSET_WHICH'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'field definition' }
SDL_ControllerButtonEvent class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Sint32 which;
    Uint8 button;
    Uint8 state;
    Uint8 padding1;
    Uint8 padding2;
 	)
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> button [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_BUTTON
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> button: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_BUTTON put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> padding1 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING1
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> padding1: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING1 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> padding2 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING2
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> padding2: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING2 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> state [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_STATE
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> state: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_STATE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> which [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_WHICH
]

{ #category : #'accessing structure variables' }
SDL_ControllerButtonEvent >> which: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_WHICH put: anObject
]
