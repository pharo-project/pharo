"
SDL2 library structure
"
Class {
	#name : #SDL_Color,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_A',
		'OFFSET_B',
		'OFFSET_G',
		'OFFSET_R'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'fields descriptions' }
SDL_Color class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^#(
    Uint8 r;
    Uint8 g;
    Uint8 b;
    Uint8 a;
 	)
]

{ #category : #'accessing structure variables' }
SDL_Color >> a [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_A
]

{ #category : #'accessing structure variables' }
SDL_Color >> a: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_A put: anObject
]

{ #category : #'accessing structure variables' }
SDL_Color >> b [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_B
]

{ #category : #'accessing structure variables' }
SDL_Color >> b: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_B put: anObject
]

{ #category : #'accessing structure variables' }
SDL_Color >> g [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_G
]

{ #category : #'accessing structure variables' }
SDL_Color >> g: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_G put: anObject
]

{ #category : #'accessing structure variables' }
SDL_Color >> r [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_R
]

{ #category : #'accessing structure variables' }
SDL_Color >> r: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_R put: anObject
]
