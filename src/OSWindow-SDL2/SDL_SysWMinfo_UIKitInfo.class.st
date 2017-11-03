"
A structure that contains UIKit specific information about a window. 
"
Class {
	#name : #SDL_SysWMinfo_UIKitInfo,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_COLORBUFFER',
		'OFFSET_FRAMEBUFFER',
		'OFFSET_RESOLVEFRAMEBUFFER',
		'OFFSET_WINDOW'
	],
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'field definition' }
SDL_SysWMinfo_UIKitInfo class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		void* window;
		uint framebuffer;
		uint colorbuffer;
		uint resolveFramebuffer;
 	)
]

{ #category : #converting }
SDL_SysWMinfo_UIKitInfo >> asPlatformSpecificHandle [
	^ OSWindowPlatformSpecificHandle new
		platform: #uikit;
		window: self window;
		surface: self framebuffer;
		yourself
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_UIKitInfo >> colorbuffer [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_COLORBUFFER
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_UIKitInfo >> colorbuffer: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_COLORBUFFER put: anObject
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_UIKitInfo >> framebuffer [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_FRAMEBUFFER
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_UIKitInfo >> framebuffer: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_FRAMEBUFFER put: anObject
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_UIKitInfo >> resolveFramebuffer [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_RESOLVEFRAMEBUFFER
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_UIKitInfo >> resolveFramebuffer: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_RESOLVEFRAMEBUFFER put: anObject
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_UIKitInfo >> window [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_WINDOW) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_UIKitInfo >> window: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_WINDOW put: anObject getHandle.
]
