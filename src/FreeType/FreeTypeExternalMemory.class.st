"
FreeType externalMemory 
"
Class {
	#name : #FreeTypeExternalMemory,
	#superclass : #FT2Handle,
	#instVars : [
		'bytes'
	],
	#category : 'FreeType-Base'
}

{ #category : #'instance creation' }
FreeTypeExternalMemory class >> bytes: aByteArray [
	| answer |
	answer := self basicNew
		bytes: aByteArray;
		yourself.
	^answer
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
	<primitive: 'primitiveCopyToExternalMemory' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #primitives }
FreeTypeExternalMemory >> primDestroyHandle [
	<primitive: 'primitiveFreeExternalMemory' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #validation }
FreeTypeExternalMemory >> validate [
	self isValid ifTrue: [ ^ self ].
	bytes ifNil: [ ^ self  ].
	
	self primCopyToExternalMemory: bytes.
	self isValid ifTrue: [ self class register: self ]
]
