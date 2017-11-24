"
A structure that contains system-dependent information about a window. 
"
Class {
	#name : #'SDL_SysWMinfo',
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_INFO',
		'OFFSET_SUBSYSTEM',
		'OFFSET_VERSION'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'field definition' }
SDL_SysWMinfo class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
		SDL_Version version;
		int subsystem;
		SDL_SysWMinfo_InfoUnion info;
 	)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo >> info [
	"This method was automatically generated"
	^SDL_SysWMinfo_InfoUnion fromHandle: (handle referenceStructAt: OFFSET_INFO length: 24)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo >> info: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_INFO put: anObject getHandle length: 24.
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo >> subsystem [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_SUBSYSTEM
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo >> subsystem: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_SUBSYSTEM put: anObject
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo >> version [
	"This method was automatically generated"
	^SDL_Version fromHandle: (handle referenceStructAt: OFFSET_VERSION length: 3)
]

{ #category : #'accessing structure variables' }
SDL_SysWMinfo >> version: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_VERSION put: anObject getHandle length: 3.
]
