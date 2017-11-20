"
I represent  handle of a mouse cursor.
"
Class {
	#name : #SDL_Cursor,
	#superclass : #SDL2Handle,
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'freeing resources' }
SDL_Cursor >> freeCursor [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_FreeCursor ( self ) )
]

{ #category : #'using the cursor' }
SDL_Cursor >> setCursor [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetCursor ( self ) )
]
