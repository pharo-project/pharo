"
I am union inside of SDL_SysWMinfo used to hold platform specific information about a window.
"
Class {
	#name : #SDL_SysWMinfo_InfoUnion,
	#superclass : #SDL2Union,
	#category : #OSWindow-SDL2-Bindings
}

{ #category : #'field definition' }
SDL_SysWMinfo_InfoUnion class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		SDL_SysWMinfo_WindowsInfo win;
		SDL_SysWMinfo_WindowsRTInfo winrt;
		SDL_SysWMinfo_X11Info x11;
		SDL_SysWMinfo_DirectFBInfo dfb;
		SDL_SysWMinfo_CocoaInfo cocoa;
		SDL_SysWMinfo_UIKitInfo uikit;
		SDL_SysWMinfo_WaylandInfo wl;
		SDL_SysWMinfo_MirInfo mir;
		SDL_SysWMinfo_AndroidInfo android;
 	)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> android [
	"This method was automatically generated"
	^SDL_SysWMinfo_AndroidInfo fromHandle: (handle referenceStructAt: 1 length: 16)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> android: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 16.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> cocoa [
	"This method was automatically generated"
	^SDL_SysWMinfo_CocoaInfo fromHandle: (handle referenceStructAt: 1 length: 8)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> cocoa: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 8.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> dfb [
	"This method was automatically generated"
	^SDL_SysWMinfo_DirectFBInfo fromHandle: (handle referenceStructAt: 1 length: 24)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> dfb: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 24.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> mir [
	"This method was automatically generated"
	^SDL_SysWMinfo_MirInfo fromHandle: (handle referenceStructAt: 1 length: 16)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> mir: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 16.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> uikit [
	"This method was automatically generated"
	^SDL_SysWMinfo_UIKitInfo fromHandle: (handle referenceStructAt: 1 length: 24)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> uikit: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 24.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> win [
	"This method was automatically generated"
	^SDL_SysWMinfo_WindowsInfo fromHandle: (handle referenceStructAt: 1 length: 16)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> win: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 16.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> winrt [
	"This method was automatically generated"
	^SDL_SysWMinfo_WindowsRTInfo fromHandle: (handle referenceStructAt: 1 length: 8)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> winrt: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 8.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> wl [
	"This method was automatically generated"
	^SDL_SysWMinfo_WaylandInfo fromHandle: (handle referenceStructAt: 1 length: 24)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> wl: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 24.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> x11 [
	"This method was automatically generated"
	^SDL_SysWMinfo_X11Info fromHandle: (handle referenceStructAt: 1 length: 16)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo_InfoUnion >> x11: anObject [
	"This method was automatically generated"
	handle structAt: 1 put: anObject getHandle length: 16.
]
