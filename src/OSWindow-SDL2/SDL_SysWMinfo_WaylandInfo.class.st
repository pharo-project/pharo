"
A structure that contains Wayland specific information about a window. 
"
Class {
	#name : #SDL_SysWMinfo_WaylandInfo,
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_DISPLAY',
		'OFFSET_SHELL_SURFACE',
		'OFFSET_SURFACE'
	],
	#category : 'OSWindow-SDL2-Bindings'
}

{ #category : #'field definition' }
SDL_SysWMinfo_WaylandInfo class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		void* display;
		void* surface;
		void* shell_surface;
 	)
]

{ #category : #converting }
SDL_SysWMinfo_WaylandInfo >> asPlatformSpecificHandle [
	^ OSWindowPlatformSpecificHandle new
		platform: #wayland;
		display: self display;
		window: self shell_surface;
		surface: self surface;
		yourself
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WaylandInfo >> display [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_DISPLAY) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WaylandInfo >> display: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_DISPLAY put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WaylandInfo >> shell_surface [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_SHELL_SURFACE) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WaylandInfo >> shell_surface: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_SHELL_SURFACE put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WaylandInfo >> surface [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_SURFACE) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_WaylandInfo >> surface: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_SURFACE put: anObject getHandle.
]
