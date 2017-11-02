"
A structure that contains Android specific information about a window. 
"
Class {
	#name : #SDL_SysWMinfo_AndroidInfo,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_SURFACE',
		'OFFSET_WINDOW'
	],
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'field definition' }
SDL_SysWMinfo_AndroidInfo class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		void* window;
		void* surface;
 	)
]

{ #category : #converting }
SDL_SysWMinfo_AndroidInfo >> asPlatformSpecificHandle [
	^ OSWindowPlatformSpecificHandle new
		platform: #android;
		window: self window;
		surface: self surface;
		yourself
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_AndroidInfo >> surface [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_SURFACE) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_AndroidInfo >> surface: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_SURFACE put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_AndroidInfo >> window [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_WINDOW) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_AndroidInfo >> window: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_WINDOW put: anObject getHandle.
]
