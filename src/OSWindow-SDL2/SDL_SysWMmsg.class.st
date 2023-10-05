"
I represent the payload of the SDL_SysWMEvent event
"
Class {
	#name : 'SDL_SysWMmsg',
	#superclass : 'SDL2Structure',
	#classVars : [
		'OFFSET_MSG',
		'OFFSET_SUBSYSTEM',
		'OFFSET_VERSION'
	],
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'field definition' }
SDL_SysWMmsg class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		SDL_Version			version
		int 					subsystem
		SDL_SysWMmsgUnion	msg
 	)
]

{ #category : 'testing' }
SDL_SysWMmsg >> isWindows [

	^ self subsystem = SDL_SYSWM_WINDOWS
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg >> msg [
	"This method was automatically generated"
	^ SDL_SysWMmsgUnion fromHandle: (handle referenceStructAt: OFFSET_MSG length: SDL_SysWMmsgUnion byteSize)
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg >> msg: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_MSG put: anObject getHandle length: SDL_SysWMmsgUnion byteSize
]

{ #category : 'printing' }
SDL_SysWMmsg >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: ' subsystem: ';
		print: self subsystem.

	self isWindows ifTrue: [
		aStream
			nextPutAll: ' Windows: ';
			print: self msg win
	].

	aStream nextPut: $)
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg >> subsystem [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_SUBSYSTEM
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg >> subsystem: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_SUBSYSTEM put: anObject
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg >> version [
	"This method was automatically generated"
	^ SDL_Version fromHandle: (handle referenceStructAt: OFFSET_VERSION length: SDL_Version byteSize)
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg >> version: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_VERSION put: anObject getHandle length: SDL_Version byteSize
]

{ #category : 'accessing - structure variables' }
SDL_SysWMmsg >> windowsMessage [

	^ self msg win
]
