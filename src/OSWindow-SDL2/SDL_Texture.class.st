"
I represent a handle of GL texture, coming from SDL2 library
"
Class {
	#name : #'SDL_Texture',
	#superclass : #SDL2Handle,
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #finalization }
SDL_Texture class >> destroyTexture: aHandle [
	^ self ffiCall: #( void SDL_DestroyTexture ( void *aHandle ) )
]

{ #category : #finalization }
SDL_Texture class >> finalizeResourceData: aHandle [
	(aHandle isNil or: [aHandle isNull]) ifTrue: [ ^ self ].
	self destroyTexture: aHandle.
	aHandle beNull	
]

{ #category : #initialization }
SDL_Texture >> alphaMod: alpha [
	^ self ffiCall: #( int SDL_SetTextureAlphaMod ( self , Uint8 alpha ) )
]

{ #category : #rendering }
SDL_Texture >> blendMode: blendMode [
	^ self ffiCall: #( int SDL_SetTextureBlendMode ( self , SDL_BlendMode blendMode ) )
]

{ #category : #initialization }
SDL_Texture >> colorModR: r g: g b: b [
	^ self ffiCall: #( int SDL_SetTextureColorMod ( self , Uint8 r, Uint8 g, Uint8 b ) )
]

{ #category : #initialization }
SDL_Texture >> destroy [
	handle isNull ifTrue: [ ^ self ].
	self primDestroy.
	handle beNull
]

{ #category : #initialization }
SDL_Texture >> initialize [
	self autoRelease
]

{ #category : #'pixels handling' }
SDL_Texture >> lockPixels: pixels pitch: pitch [
	^ self ffiCall: #( int SDL_LockTexture ( self , nil , void** pixels , int* pitch ) )
]

{ #category : #private }
SDL_Texture >> primDestroy [
	^ self ffiCall: #( void SDL_DestroyTexture ( self ) )
]

{ #category : #'pixels handling' }
SDL_Texture >> unlock [
	^ self ffiCall: #( void SDL_UnlockTexture ( self ) )
]

{ #category : #initialization }
SDL_Texture >> updateTexturePixels: pixels pitch: pitch [
	^ self ffiCall: #( int SDL_UpdateTexture ( self , nil , void* pixels , int pitch ) )
]

{ #category : #initialization }
SDL_Texture >> updateTextureRect: rect pixels: pixels pitch: pitch [
	^ self ffiCall: #( int SDL_UpdateTexture ( self , SDL_Rect* rect , void* pixels , int pitch ) )
]
