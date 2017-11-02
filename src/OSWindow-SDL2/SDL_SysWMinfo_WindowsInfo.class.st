"
A structure that contains Window specific information about a window. 
"
Class {
	#name : #SDL_SysWMinfo_WindowsInfo,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_HDC',
		'OFFSET_WINDOW'
	],
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'field definition' }
SDL_SysWMinfo_WindowsInfo class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		void* window;
		void* hdc;
 	)
]

{ #category : #converting }
SDL_SysWMinfo_WindowsInfo >> asPlatformSpecificHandle [
	^ OSWindowPlatformSpecificHandle new
		platform: #win32;
		window: self window;
		surface: self hdc;
		yourself
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WindowsInfo >> hdc [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_HDC) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WindowsInfo >> hdc: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_HDC put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WindowsInfo >> window [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_WINDOW) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WindowsInfo >> window: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_WINDOW put: anObject getHandle.
]
