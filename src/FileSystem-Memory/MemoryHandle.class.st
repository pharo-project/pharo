"
I provide ""primitives"" for doing IO on files in a MemoryFileSystem. I delegate most of my actions to the MemoryFile. This way there is only one place needed where the data is stored.
"
Class {
	#name : #MemoryHandle,
	#superclass : #FileSystemHandle,
	#instVars : [
		'entry'
	],
	#category : #'FileSystem-Memory-Base'
}

{ #category : #public }
MemoryHandle >> at: index [
	^ entry at: index
]

{ #category : #public }
MemoryHandle >> at: index put: anObject [
	^ entry at: index put: anObject
]

{ #category : #public }
MemoryHandle >> at: index read: aCollection startingAt: start count: count [ 
	^ entry at: index read: aCollection startingAt: start count: count 
]

{ #category : #public }
MemoryHandle >> at: first write: aCollection startingAt: start count: count [ 
	writable ifFalse: [ self primitiveFailed ].
	entry at: first write: aCollection startingAt: start count: count.
]

{ #category : #streams }
MemoryHandle >> binaryReadStream [

	^ entry binaryReadStream
]

{ #category : #streams }
MemoryHandle >> binaryWriteStream [
	
	^ entry binaryWriteStream
]

{ #category : #public }
MemoryHandle >> close [
	self isOpen ifFalse: [ ^ self ].
	self truncate.
	entry := nil.
]

{ #category : #'stream-protocol' }
MemoryHandle >> copyFrom: from to: position [
	^ entry copyFrom: from to: position
]

{ #category : #public }
MemoryHandle >> flush [
	self truncate
]

{ #category : #private }
MemoryHandle >> grow [
	entry grow
]

{ #category : #'stream-protocol' }
MemoryHandle >> grownBy: length [
	entry grownBy: length
]

{ #category : #testing }
MemoryHandle >> isOpen [
	^ entry notNil
]

{ #category : #public }
MemoryHandle >> open [
	entry := self basicOpen.
]

{ #category : #accessing }
MemoryHandle >> size [
	"return the size for the interna"
	^ entry internalSize
]

{ #category : #public }
MemoryHandle >> sync [
	self flush
]

{ #category : #public }
MemoryHandle >> truncate [
	entry truncate
]

{ #category : #public }
MemoryHandle >> truncateTo: anInteger [
	entry truncateTo: anInteger
]
