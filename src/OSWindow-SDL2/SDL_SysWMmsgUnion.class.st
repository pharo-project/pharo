"
I represent the union that is inside the SDL_SysWMmsg structure.
I have inside different structures depending on the platform executing.
"
Class {
	#name : 'SDL_SysWMmsgUnion',
	#superclass : 'SDL2Union',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'field definition' }
SDL_SysWMmsgUnion class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		SDL_SysWMmsg_Windows win
 	)
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsgUnion >> win [
	"This method was automatically generated"
	^ SDL_SysWMmsg_Windows fromHandle: (handle referenceStructAt: 1 length: SDL_SysWMmsg_Windows byteSize)
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsgUnion >> win: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 32.
]
