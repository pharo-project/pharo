"
This is the specific message structure used when receiving a SDL_SysWMmsg
"
Class {
	#name : 'SDL_SysWMmsg_Windows',
	#superclass : 'SDL2Structure',
	#classVars : [
		'OFFSET_HWND',
		'OFFSET_LPARAM',
		'OFFSET_MSG',
		'OFFSET_WPARAM'
	],
	#pools : [
		'SDL2WindowsMessages'
	],
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'field definition' }
SDL_SysWMmsg_Windows class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		void* hwnd
		uint32 msg
		uint64 wParam
		uint64 lParam
 	)
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg_Windows >> hwnd [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_HWND) type: ExternalType void asPointerType
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg_Windows >> hwnd: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_HWND put: anObject getHandle.
]

{ #category : 'testing' }
SDL_SysWMmsg_Windows >> isDisplayChanged [

	^ self msg = WM_DISPLAYCHANGE
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg_Windows >> lParam [
	"This method was automatically generated"
	^handle unsignedLongLongAt: OFFSET_LPARAM
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg_Windows >> lParam: anObject [
	"This method was automatically generated"
	handle unsignedLongLongAt: OFFSET_LPARAM put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg_Windows >> msg [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_MSG
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg_Windows >> msg: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_MSG put: anObject
]

{ #category : 'printing' }
SDL_SysWMmsg_Windows >> printOn: aStream [

	super printOn: aStream.

	aStream
		nextPutAll: ' msg: ';
		print: (SDL2WindowsMessages translateId: self msg) asString;
		nextPutAll: ' wParam: ';
		print: self wParam;
		nextPutAll: ' lParam: ';
		print: self lParam
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg_Windows >> wParam [
	"This method was automatically generated"
	^handle unsignedLongLongAt: OFFSET_WPARAM
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg_Windows >> wParam: anObject [
	"This method was automatically generated"
	handle unsignedLongLongAt: OFFSET_WPARAM put: anObject
]
