"
SDL2 library structure
"
Class {
	#name : #'SDL_Surface',
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_CLIP_RECT',
		'OFFSET_FLAGS',
		'OFFSET_FORMAT',
		'OFFSET_H',
		'OFFSET_LOCKED',
		'OFFSET_LOCK_DATA',
		'OFFSET_MAP',
		'OFFSET_PITCH',
		'OFFSET_PIXELS',
		'OFFSET_REFCOUNT',
		'OFFSET_USERDATA',
		'OFFSET_W'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'fields description' }
SDL_Surface class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^#(
		Uint32 flags;
		SDL_PixelFormat* format;
    		int w;
		int h;
    		int pitch;
    		void* pixels;
		void* userdata;
		int locked;
		void* lock_data;
		SDL_Rect clip_rect;
    		void *map;
    		int refcount;
 	)
]

{ #category : #finalization }
SDL_Surface class >> finalizeResourceData: aHandle [
	aHandle isNull ifTrue: [ ^ self ].
	self freeSurface: aHandle.
	aHandle beNull
]

{ #category : #video }
SDL_Surface class >> freeSurface: aHandle [
	^ self ffiCall: #( void SDL_FreeSurface ( void *aHandle ) )
]

{ #category : #'pixel accessing' }
SDL_Surface >> SDL_LockSurface: aSurface [
	^ self ffiCall: #(int SDL_LockSurface( SDL_Surface* aSurface ))
]

{ #category : #'pixel accessing' }
SDL_Surface >> SDL_UnlockSurface: aSurface [
	^ self ffiCall: #( int SDL_UnlockSurface( SDL_Surface* aSurface ) )
]

{ #category : #'accessing structure variables' }
SDL_Surface >> clip_rect [
	"This method was automatically generated"
	^SDL_Rect fromHandle: (handle referenceStructAt: OFFSET_CLIP_RECT length: 16)
]

{ #category : #'accessing structure variables' }
SDL_Surface >> clip_rect: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_CLIP_RECT put: anObject getHandle length: 16.
]

{ #category : #'accessing structure variables' }
SDL_Surface >> flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_FLAGS
]

{ #category : #'accessing structure variables' }
SDL_Surface >> flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_FLAGS put: anObject
]

{ #category : #'accessing structure variables' }
SDL_Surface >> format [
	"This method was automatically generated"
	^SDL_PixelFormat fromHandle: (handle pointerAt: OFFSET_FORMAT)
]

{ #category : #'accessing structure variables' }
SDL_Surface >> format: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_FORMAT put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_Surface >> h [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_H
]

{ #category : #'accessing structure variables' }
SDL_Surface >> h: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_H put: anObject
]

{ #category : #initialization }
SDL_Surface >> initialize [
	self autoRelease
]

{ #category : #'pixel accessing' }
SDL_Surface >> lockSurface [
	^ self SDL_LockSurface: self
]

{ #category : #'accessing structure variables' }
SDL_Surface >> lock_data [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_LOCK_DATA) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_Surface >> lock_data: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_LOCK_DATA put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_Surface >> locked [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_LOCKED
]

{ #category : #'accessing structure variables' }
SDL_Surface >> locked: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_LOCKED put: anObject
]

{ #category : #'accessing structure variables' }
SDL_Surface >> map [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_MAP) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_Surface >> map: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_MAP put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_Surface >> pitch [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_PITCH
]

{ #category : #'accessing structure variables' }
SDL_Surface >> pitch: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_PITCH put: anObject
]

{ #category : #'accessing structure variables' }
SDL_Surface >> pixels [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_PIXELS) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_Surface >> pixels: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_PIXELS put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_Surface >> refcount [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_REFCOUNT
]

{ #category : #'accessing structure variables' }
SDL_Surface >> refcount: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_REFCOUNT put: anObject
]

{ #category : #'pixel accessing' }
SDL_Surface >> unlockSurface [
	^ self SDL_UnlockSurface: self
]

{ #category : #'accessing structure variables' }
SDL_Surface >> userdata [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_USERDATA) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_Surface >> userdata: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_USERDATA put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_Surface >> w [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_W
]

{ #category : #'accessing structure variables' }
SDL_Surface >> w: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_W put: anObject
]
