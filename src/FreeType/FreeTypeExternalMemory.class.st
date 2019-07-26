"
FreeType externalMemory 
"
Class {
	#name : #FreeTypeExternalMemory,
	#superclass : #FT2Handle,
	#instVars : [
		'bytes'
	],
	#category : #'FreeType-Base'
}

{ #category : #'instance creation' }
FreeTypeExternalMemory class >> bytes: aByteArray [
	^ self basicNew
		bytes: aByteArray;
		autoRelease;
		yourself
]

{ #category : #finalization }
FreeTypeExternalMemory class >> finalizeResourceData: handle [

	handle ifNil: [ ^ self ].
	handle isNull
		ifTrue: [ ^ self ].
	handle free.
	handle beNull
]

{ #category : #'instance creation' }
FreeTypeExternalMemory class >> new [
	self error: 'Use #bytes:'
]

{ #category : #accessing }
FreeTypeExternalMemory >> bytes [
	^bytes
]

{ #category : #accessing }
FreeTypeExternalMemory >> bytes: aByteArray [
	bytes := aByteArray
]

{ #category : #primitives }
FreeTypeExternalMemory >> primCopyToExternalMemory: aByteArray [
	"copy aByteArray into newly allocated, external memory, and store the
	address of that memory in the receiver's handle"

	handle := ExternalAddress allocate: aByteArray size.
	LibC memCopy: aByteArray to: handle size: aByteArray size.
]

{ #category : #validation }
FreeTypeExternalMemory >> validate [
	self isValid ifTrue: [ ^ self ].
	bytes ifNil: [ ^ self  ].
	
	self primCopyToExternalMemory: bytes.
	self autoRelease

]
