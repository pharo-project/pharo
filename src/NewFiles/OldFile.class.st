"
I provide legacy file plugin support.
"
Class {
	#name : 'OldFile',
	#superclass : 'AbstractFile',
	#instVars : [
		'isForWrite'
	],
	#category : 'NewFiles',
	#package : 'NewFiles'
}

{ #category : 'accessing' }
OldFile class >> apiName [
	^ 'OldFile'
]

{ #category : 'primitives - files' }
OldFile class >> checkHandle: handle forPath: path [
	handle ifNil: [ 
		self error: 'Failed to open file ' , path printString
	].
]

{ #category : 'primitives - path' }
OldFile class >> deleteFile: path [
	^ File deleteFile: path
]

{ #category : 'public' }
OldFile class >> exists: aPath [
	^ File exists: aPath
]

{ #category : 'primitives - files' }
OldFile class >> finalizeResourceData: aHandle [
	(File closed: aHandle ) ifTrue: [ ^ self ].
	File close: aHandle
]

{ #category : 'primitives - files' }
OldFile class >> makeWithHandle: handle forWrite: isForWrite [
	| file |
	file := self fromHandle: handle.
	file isForWrite: isForWrite.
	^ file autoRelease
]

{ #category : 'public' }
OldFile class >> open: aReference writable: isWriteable [
	| path file |
	path := aReference fullName.
	file := isWriteable
		ifTrue: [ self openAlways: path  ]
		ifFalse: [ self openReadOnly: path]. 
	file ifNotNil: [ file setReference: aReference ].
	^ file
]

{ #category : 'primitives - files' }
OldFile class >> openAlways: path [

	| handle |
	handle := File open: path writable: true.
	self checkHandle: handle forPath: path.
	^ self makeWithHandle: handle forWrite: true
]

{ #category : 'primitives - files' }
OldFile class >> openCreateAlways: path [

	| handle |
	handle := File open: path writable: true.
	self checkHandle: handle forPath: path.
	File truncate: handle to: 0.
	^ self makeWithHandle: handle forWrite: true
]

{ #category : 'primitives - files' }
OldFile class >> openReadOnly: path [
	| handle |
	handle := File open: path writable: false.
	self checkHandle: handle forPath: path.
	^ self makeWithHandle: handle forWrite: false
]

{ #category : 'accessing' }
OldFile >> atEnd [
	^ self position >= self size
]

{ #category : 'initialization' }
OldFile >> close [
	File close: handle
]

{ #category : 'accessing' }
OldFile >> contents [
	| buffer |
	buffer := ByteArray new: self size.
	self readInto: buffer at: 0.
	^ buffer
]

{ #category : 'accessing' }
OldFile >> isForWrite [

	^ isForWrite
]

{ #category : 'accessing' }
OldFile >> isForWrite: anObject [

	isForWrite := anObject
]

{ #category : 'accessing' }
OldFile >> position [
	^ File getPosition: handle
]

{ #category : 'accessing' }
OldFile >> position: newPosition [
	^ File setPosition: handle to: newPosition
]

{ #category : 'write and read' }
OldFile >> readInto: buffer [
	^ self readInto: buffer offset: 0 withSize: buffer size
]

{ #category : 'accessing' }
OldFile >> readInto: buffer at: fileOffset [
	^ self readInto: buffer offset: 0 withSize: buffer size at: fileOffset
]

{ #category : 'write and read' }
OldFile >> readInto: buffer offset: bufferOffset withSize: readSize [
	^ File read: handle into: buffer startingAt: bufferOffset + 1 count: readSize
]

{ #category : 'accessing' }
OldFile >> readInto: buffer offset: bufferOffset withSize: readSize at: fileOffset [
	^ self withPosition: fileOffset during: [
		self readInto: buffer offset: bufferOffset withSize: readSize
	]
]

{ #category : 'write and read' }
OldFile >> readInto: buffer withSize: readSize [
	^ self readInto: buffer offset: 0 withSize: readSize
]

{ #category : 'accessing' }
OldFile >> size [
	^ File sizeOf: handle
]

{ #category : 'accessing' }
OldFile >> truncate [
	^ self truncateToSize: 0
]

{ #category : 'accessing' }
OldFile >> truncateToSize: newSize [
	^ File truncate: handle to: newSize
]

{ #category : 'accessing' }
OldFile >> withPosition: newPosition during: aBlock [
	| oldPosition |
	oldPosition := self position.
	self position: newPosition.
	
	^ aBlock ensure: [ 
		self position: oldPosition
	]
]

{ #category : 'write and read' }
OldFile >> writeFrom: dataBuffer [
	^ self writeFrom: dataBuffer offset: 0 withSize: dataBuffer size
]

{ #category : 'writing' }
OldFile >> writeFrom: dataBuffer at: fileOffset [
	^ self writeFrom: dataBuffer offset: 0 withSize: dataBuffer size at: fileOffset
]

{ #category : 'write and read' }
OldFile >> writeFrom: buffer offset: bufferOffset withSize: bufferSize [
	^ File write: handle from: buffer startingAt: bufferOffset + 1 count: bufferSize
]

{ #category : 'writing' }
OldFile >> writeFrom: buffer offset: bufferOffset withSize: bufferSize at: fileOffset [
	^ self withPosition: fileOffset during: [
		self writeFrom: buffer offset: bufferOffset withSize: bufferSize
	]
]

{ #category : 'write and read' }
OldFile >> writeFrom: buffer withSize: bufferSize [
	^ self writeFrom: buffer offset: 0 withSize: bufferSize
]
