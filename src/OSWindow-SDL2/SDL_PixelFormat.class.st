"
SDL2 library structure
"
Class {
	#name : #'SDL_PixelFormat',
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_ALOSS',
		'OFFSET_AMASK',
		'OFFSET_ASHIFT',
		'OFFSET_BITSPERPIXEL',
		'OFFSET_BLOSS',
		'OFFSET_BMASK',
		'OFFSET_BSHIFT',
		'OFFSET_BYTESPERPIXEL',
		'OFFSET_FORMAT',
		'OFFSET_GLOSS',
		'OFFSET_GMASK',
		'OFFSET_GSHIFT',
		'OFFSET_NEXT',
		'OFFSET_PAD1',
		'OFFSET_PAD2',
		'OFFSET_PALETTE',
		'OFFSET_REFCOUNT',
		'OFFSET_RLOSS',
		'OFFSET_RMASK',
		'OFFSET_RSHIFT'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'fields description' }
SDL_PixelFormat class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^#(
    Uint32 format;
    SDL_Palette* palette;
    Uint8 BitsPerPixel;
    Uint8 BytesPerPixel;
    Uint8 pad1;
	 Uint8 pad2;
    Uint32 Rmask;
    Uint32 Gmask;
    Uint32 Bmask;
    Uint32 Amask;
    Uint8 Rloss;
    Uint8 Gloss;
    Uint8 Bloss;
    Uint8 Aloss;
    Uint8 Rshift;
    Uint8 Gshift;
    Uint8 Bshift;
    Uint8 Ashift;
    int refcount;
    SDL_PixelFormat* next;
 	)
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Aloss [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_ALOSS
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Aloss: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_ALOSS put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Amask [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_AMASK
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Amask: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_AMASK put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Ashift [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_ASHIFT
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Ashift: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_ASHIFT put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> BitsPerPixel [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_BITSPERPIXEL
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> BitsPerPixel: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_BITSPERPIXEL put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Bloss [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_BLOSS
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Bloss: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_BLOSS put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Bmask [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_BMASK
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Bmask: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_BMASK put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Bshift [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_BSHIFT
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Bshift: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_BSHIFT put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> BytesPerPixel [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_BYTESPERPIXEL
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> BytesPerPixel: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_BYTESPERPIXEL put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Gloss [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_GLOSS
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Gloss: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_GLOSS put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Gmask [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_GMASK
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Gmask: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_GMASK put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Gshift [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_GSHIFT
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Gshift: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_GSHIFT put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Rloss [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_RLOSS
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Rloss: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_RLOSS put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Rmask [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_RMASK
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Rmask: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_RMASK put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Rshift [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_RSHIFT
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> Rshift: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_RSHIFT put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> format [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_FORMAT
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> format: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_FORMAT put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> next [
	"This method was automatically generated"
	^SDL_PixelFormat fromHandle: (handle pointerAt: OFFSET_NEXT)
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> next: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_NEXT put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> pad1 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PAD1
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> pad1: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PAD1 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> pad2 [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_PAD2
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> pad2: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_PAD2 put: anObject
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> palette [
	"This method was automatically generated"
	^SDL_Palette fromHandle: (handle pointerAt: OFFSET_PALETTE)
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> palette: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PALETTE put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> refcount [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_REFCOUNT
]

{ #category : #'accessing structure variables' }
SDL_PixelFormat >> refcount: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_REFCOUNT put: anObject
]
