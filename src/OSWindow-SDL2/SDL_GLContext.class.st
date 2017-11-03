"
I represent a handle of GL rendering context, coming from SDL2 library
"
Class {
	#name : #SDL_GLContext,
	#superclass : #SDL2Handle,
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #deleting }
SDL_GLContext >> delete [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_GL_DeleteContext ( self ) )
]
