"
A structure that contains Cocoa specific information about a window. 
"
Class {
	#name : #SDL_SysWMinfo_CocoaInfo,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_WINDOW'
	],
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'field definition' }
SDL_SysWMinfo_CocoaInfo class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		void* window;
 	)
]

{ #category : #converting }
SDL_SysWMinfo_CocoaInfo >> asPlatformSpecificHandle [
	^ OSWindowPlatformSpecificHandle new
		platform: #cocoa;
		window: self window;
		yourself
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_CocoaInfo >> window [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_WINDOW) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_CocoaInfo >> window: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_WINDOW put: anObject getHandle.
]
