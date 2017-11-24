"
A structure that contains DirectFB specific information about a window. 
"
Class {
	#name : #'SDL_SysWMinfo_DirectFBInfo',
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_DFB',
		'OFFSET_SURFACE',
		'OFFSET_WINDOW'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'field definition' }
SDL_SysWMinfo_DirectFBInfo class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		void* dfb;
		void* window;
		void* surface;
 	)
]

{ #category : #converting }
SDL_SysWMinfo_DirectFBInfo >> asPlatformSpecificHandle [
	^ OSWindowPlatformSpecificHandle new
		platform: #directfb;
		display: self dfb;
		window: self window;
		surface: self surface;
		yourself
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_DirectFBInfo >> dfb [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_DFB) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_DirectFBInfo >> dfb: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_DFB put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_DirectFBInfo >> surface [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_SURFACE) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_DirectFBInfo >> surface: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_SURFACE put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_DirectFBInfo >> window [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_WINDOW) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_DirectFBInfo >> window: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_WINDOW put: anObject getHandle.
]
