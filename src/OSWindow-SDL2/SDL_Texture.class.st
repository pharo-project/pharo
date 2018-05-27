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
	aHandle isNull ifTrue: [ ^ self ].
	self destroyTexture: aHandle.
	aHandle beNull	
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
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_LockTexture ( self , nil , void** pixels , int* pitch ) )
]

{ #category : #private }
SDL_Texture >> primDestroy [
	^ self ffiCall: #( void SDL_DestroyTexture ( self ) )
]

{ #category : #'pixels handling' }
SDL_Texture >> unlock [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_UnlockTexture ( self ) )
]

{ #category : #initialization }
SDL_Texture >> updateTexturePixels: pixels pitch: pitch [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_UpdateTexture ( self , nil , void* pixels , int pitch ) )
]

{ #category : #initialization }
SDL_Texture >> updateTextureRect: rect pixels: pixels pitch: pitch [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_UpdateTexture ( self , SDL_Rect* rect , void* pixels , int pitch ) )
]
