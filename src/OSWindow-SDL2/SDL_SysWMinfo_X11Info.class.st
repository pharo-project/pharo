"
A structure that contains X11 specific information about a window. 
"
Class {
	#name : #SDL_SysWMinfo_X11Info,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_DISPLAY',
		'OFFSET_WINDOW'
	],
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'field definition' }
SDL_SysWMinfo_X11Info class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		void* display;
		void* window;
 	)
]

{ #category : #converting }
SDL_SysWMinfo_X11Info >> asPlatformSpecificHandle [
	^ OSWindowPlatformSpecificHandle new
		platform: #x11;
		display: self display;
		window: self window;
		yourself
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_X11Info >> display [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_DISPLAY) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_X11Info >> display: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_DISPLAY put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_X11Info >> window [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_WINDOW) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_X11Info >> window: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_WINDOW put: anObject getHandle.
]
