"
A structure that contains information on the capabilities of a render driver or the current render context.

https://wiki.libsdl.org/SDL_RendererInfo
"
Class {
	#name : #SDL_RendererInfo,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_FLAGS',
		'OFFSET_MAX_TEXTURE_HEIGHT',
		'OFFSET_MAX_TEXTURE_WIDTH',
		'OFFSET_NAME',
		'OFFSET_NUM_TEXTURE_FORMATS',
		'OFFSET_TEXTURE_FORMATS',
		'Uint32_16'
	],
	#pools : [
		'SDL2Types'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'field definition' }
SDL_RendererInfo class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
   	 	char *name;
    	Uint32 flags;
    	Uint32 num_texture_formats;
    	Uint32_16 texture_formats;
    	int max_texture_width;
    	int max_texture_height;
 	)
]

{ #category : #'class initialization' }
SDL_RendererInfo class >> initialize [
	Uint32_16 := FFITypeArray ofType: Uint32 size: 16
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> flags [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_FLAGS
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> flags: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_FLAGS put: anObject
]

{ #category : #testing }
SDL_RendererInfo >> isAccelerated [
	^ self flags & SDL_RENDERER_ACCELERATED == SDL_RENDERER_ACCELERATED
]

{ #category : #testing }
SDL_RendererInfo >> isSoftware [
	^ self flags & SDL_RENDERER_SOFTWARE == SDL_RENDERER_SOFTWARE
]

{ #category : #testing }
SDL_RendererInfo >> isSupportRenderTexture [
	^ self flags & SDL_RENDERER_TARGETTEXTURE == SDL_RENDERER_TARGETTEXTURE
]

{ #category : #testing }
SDL_RendererInfo >> isSynchronizedWithRefreshRate [
	^ self flags & SDL_RENDERER_PRESENTVSYNC == SDL_RENDERER_PRESENTVSYNC
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> max_texture_height [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_MAX_TEXTURE_HEIGHT
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> max_texture_height: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_MAX_TEXTURE_HEIGHT put: anObject
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> max_texture_width [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_MAX_TEXTURE_WIDTH
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> max_texture_width: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_MAX_TEXTURE_WIDTH put: anObject
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> name [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_NAME) type: ExternalType char asPointerType
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> name: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_NAME put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> num_texture_formats [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_NUM_TEXTURE_FORMATS
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> num_texture_formats: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_NUM_TEXTURE_FORMATS put: anObject
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> texture_formats [
	"This method was automatically generated"
	^(FFITypeArray ofType: #FFIUInt32 size: 16) fromHandle: (handle copyFrom: OFFSET_TEXTURE_FORMATS to: OFFSET_TEXTURE_FORMATS + 63)
]

{ #category : #'accessing structure variables' }
SDL_RendererInfo >> texture_formats: anObject [
	"This method was automatically generated"
	handle replaceFrom: OFFSET_TEXTURE_FORMATS to: OFFSET_TEXTURE_FORMATS + 63 with: anObject getHandle startingAt: 1
]
