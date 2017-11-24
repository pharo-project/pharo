"
SDL2 library structure representing a joystick axis event data.
"
Class {
	#name : #'SDL_JoyAxisEvent',
	#superclass : #SDL2MappedEvent,
	#classVars : [
		'OFFSET_AXIS',
		'OFFSET_PADDING1',
		'OFFSET_PADDING2',
		'OFFSET_PADDING3',
		'OFFSET_PADDING4',
		'OFFSET_TIMESTAMP',
		'OFFSET_TYPE',
		'OFFSET_VALUE',
		'OFFSET_WHICH'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'event type' }
SDL_JoyAxisEvent class >> eventType [
	^ SDL_JOYAXISMOTION 
]

{ #category : #'fields description' }
SDL_JoyAxisEvent class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Sint32 which;
    Uint8 axis;
    Uint8 padding1;
    Uint8 padding2;
    Uint8 padding3;
    Sint16 value;
    Uint16 padding4;
 	)
]

{ #category : #visitor }
SDL_JoyAxisEvent >> accept: aVisitor [
	^ aVisitor visitJoyAxisEvent: self
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> axis [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_AXIS
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> axis: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_AXIS put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> padding1 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING1
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> padding1: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING1 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> padding2 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING2
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> padding2: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING2 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> padding3 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PADDING3
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> padding3: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PADDING3 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> padding4 [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PADDING4
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> padding4: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PADDING4 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> value [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_VALUE
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> value: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_VALUE put: anObject
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> which [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_WHICH
]

{ #category : #'accessing structure variables' }
SDL_JoyAxisEvent >> which: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_WHICH put: anObject
]
