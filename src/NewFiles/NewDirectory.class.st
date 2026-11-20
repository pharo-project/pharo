"
I am a directory stream.
"
Class {
	#name : 'NewDirectory',
	#superclass : 'FFIExternalObject',
	#pools : [
		'NewFileConstants'
	],
	#category : 'NewFiles',
	#package : 'NewFiles'
}

{ #category : 'archive operations' }
NewDirectory class >> contentsOf: path [
	| directory |
	directory := self open: path.
	directory ifNil: [
		self error: 'Failed to open directory ' , path
	].

	^ [ 
		directory contents.
	] ensure: [ 
		directory close
	]
]

{ #category : 'creation and destruction' }
NewDirectory class >> create: path [
	<primitive: 'primitiveDirectoryCreate' module: 'NewFilePlugin'>
	^ self primitiveFailed 
]

{ #category : 'finalization' }
NewDirectory class >> finalizeResourceData: aHandle [
	aHandle isNull ifTrue: [ ^ self ].
	self primitiveClose: aHandle.
	aHandle beNull
]

{ #category : 'creation and destruction' }
NewDirectory class >> open: path [
	| handle directory |
	handle := self primitiveOpen: path.
	handle isNull ifTrue: [ ^ nil ].
	
	directory := self fromHandle: handle.
	^ directory autoRelease
]

{ #category : 'private' }
NewDirectory class >> primitiveClose: aDirectory [
	<primitive: 'primitiveDirectoryClose' module: 'NewFilePlugin'>
	^ self primitiveFailed 
	
]

{ #category : 'private' }
NewDirectory class >> primitiveOpen: path [
	<primitive: 'primitiveDirectoryOpen' module: 'NewFilePlugin'>
	^ self primitiveFailed 
]

{ #category : 'creation and destruction' }
NewDirectory class >> removeEmpty: path [
	<primitive: 'primitiveDirectoryRemoveEmpty' module: 'NewFilePlugin'>
	^ self primitiveFailed 
	
]

{ #category : 'initialization' }
NewDirectory >> close [
	handle isNull ifTrue: [ ^ self ].
	self primitiveClose: handle.
	handle beNull
]

{ #category : 'private' }
NewDirectory >> contents [
	^ Array streamContents: [:out |
		| next	 |
		self rewind.
		[ (next := self next) ~~ nil ] whileTrue: [
			out nextPut: next
		]	
	]
]

{ #category : 'private' }
NewDirectory >> next [
	| nextNameAddress nextName |
	nextNameAddress := self primitiveNext: handle.
	nextNameAddress isNull ifTrue: [^ nil ].
	
	nextName := nextNameAddress utf8StringFromCString.
	nextName ifEmpty: [ ^ nil ].
	
	^ nextName 
]

{ #category : 'private' }
NewDirectory >> primitiveClose: aDirectory [
	<primitive: 'primitiveDirectoryClose' module: 'NewFilePlugin'>
	^ self primitiveFailed 
	
]

{ #category : 'private' }
NewDirectory >> primitiveNext: aHandle [
	<primitive: 'primitiveDirectoryNext' module: 'NewFilePlugin'>
	^ self primitiveFailed 
	
]

{ #category : 'private' }
NewDirectory >> primitiveRewind: aHandle [
	<primitive: 'primitiveDirectoryRewind' module: 'NewFilePlugin'>
	^ self primitiveFailed 
	
]

{ #category : 'private' }
NewDirectory >> rewind [
	^ self primitiveRewind: handle
]
