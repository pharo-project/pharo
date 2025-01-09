"
I represent the SDL_SysWMEvent, this event represent a system window event
"
Class {
	#name : 'SDL_SysWMEvent',
	#superclass : 'SDL2MappedEvent',
	#instVars : [
		'windowId'
	],
	#classVars : [
		'OFFSET_MSG',
		'OFFSET_TIMESTAMP',
		'OFFSET_TYPE'
	],
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_SysWMEvent class >> eventType [

	^ SDL_SYSWMEVENT
]

{ #category : 'field description' }
SDL_SysWMEvent class >> fieldsDesc [
	"
		self rebuildFieldAccessors
	"

	^ #(
    Uint32 type
    Uint32 timestamp
	 SDL_SysWMmsg*  msg
 	)
]

{ #category : 'visitor' }
SDL_SysWMEvent >> accept: aVisitor [

	self isWindows ifFalse: [ ^ nil ].
	self windowsMessage isDisplayChanged ifFalse: [ ^ nil ].

	^ aVisitor visitSystemWindowEvent: self
]

{ #category : 'event type' }
SDL_SysWMEvent >> eventType [

	^ SDL_SYSWMEVENT
]

{ #category : 'testing' }
SDL_SysWMEvent >> isWindows [

	^ self msg isWindows
]

{ #category : 'accessing - structure variables' }
SDL_SysWMEvent >> msg [
	"This method was automatically generated"
	^SDL_SysWMmsg fromHandle: (handle pointerAt: OFFSET_MSG)
]

{ #category : 'accessing - structure variables' }
SDL_SysWMEvent >> msg: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_MSG put: anObject getHandle.
]

{ #category : 'printing' }
SDL_SysWMEvent >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $:;
		print: self msg
]

{ #category : 'accessing - structure variables' }
SDL_SysWMEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : 'accessing - structure variables' }
SDL_SysWMEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_SysWMEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : 'accessing - structure variables' }
SDL_SysWMEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : 'accessing' }
SDL_SysWMEvent >> windowID [

	self isWindows
		ifFalse: [ ^ nil ].
	self windowsMessage isDisplayChanged ifFalse: [ ^ nil ].

	^ windowId ifNil: [
		  OSSDL2Driver allWindows
			  detect: [ :w |
			  w platformSpecificHandle window getHandle = self windowsMessage hwnd getHandle ]
			  ifFound: [ :w | ^ windowId := w windowId ]
			  ifNone: [ ^ nil ] ]
]

{ #category : 'accessing' }
SDL_SysWMEvent >> windowsMessage [

	^ self msg windowsMessage
]
