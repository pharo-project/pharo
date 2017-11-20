"
A structure that contains Mir specific information about a window. 
"
Class {
	#name : #SDL_SysWMinfo_MirInfo,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_CONNECTION',
		'OFFSET_SURFACE'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'field definition' }
SDL_SysWMinfo_MirInfo class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		void* connection;
		void* surface;
 	)
]

{ #category : #converting }
SDL_SysWMinfo_MirInfo >> asPlatformSpecificHandle [
	^ OSWindowPlatformSpecificHandle new
		platform: #mir;
		display: self connection;
		surface: self surface;
		yourself
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_MirInfo >> connection [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_CONNECTION) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_MirInfo >> connection: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_CONNECTION put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_MirInfo >> surface [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_SURFACE) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_MirInfo >> surface: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_SURFACE put: anObject getHandle.
]
