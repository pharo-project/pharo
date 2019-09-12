"
A file write stream - but within memory
"
Class {
	#name : #MemoryFileWriteStream,
	#superclass : #Stream,
	#instVars : [
		'file',
		'stream'
	],
	#category : #'FileSystem-Memory-Streams'
}

{ #category : #'instance creation' }
MemoryFileWriteStream class >> on: aFile [

	^ self basicNew
		file: aFile;
		yourself
]

{ #category : #testing }
MemoryFileWriteStream >> atEnd [
	self shouldNotImplement 
]

{ #category : #'opening-closing' }
MemoryFileWriteStream >> close [
	self flush.
	self stream close.
	file close
]

{ #category : #'opening-closing' }
MemoryFileWriteStream >> closed [
	^ file closed
]

{ #category : #accessing }
MemoryFileWriteStream >> contents [
	self shouldNotImplement 
]

{ #category : #accessing }
MemoryFileWriteStream >> file: aMemoryFileSystemFile [ 
	file := aMemoryFileSystemFile
]

{ #category : #writing }
MemoryFileWriteStream >> flush [
	| contents |
	contents := self stream contents.
	(contents size > 0)
		ifTrue: [ file updateContents: contents ]
]

{ #category : #testing }
MemoryFileWriteStream >> isBinary [
	^ self stream isBinary
]

{ #category : #accessing }
MemoryFileWriteStream >> next [
	self shouldNotImplement  
]

{ #category : #writing }
MemoryFileWriteStream >> nextPut: aCollection [
	^ self stream nextPut: aCollection
]

{ #category : #writing }
MemoryFileWriteStream >> nextPutAll: aCollection [
	^ self stream nextPutAll: aCollection
]

{ #category : #positioning }
MemoryFileWriteStream >> position [
	^ self stream position
]

{ #category : #positioning }
MemoryFileWriteStream >> position: anInteger [
	"Set the current position for accessing the objects to be anInteger, as long 
	as anInteger is within the bounds of the receiver's contents. If it is not, 
	create an error notification."

	stream position: anInteger
]

{ #category : #positioning }
MemoryFileWriteStream >> setToEnd [
	^ self stream setToEnd
]

{ #category : #accessing }
MemoryFileWriteStream >> size [
	^ file size
]

{ #category : #accessing }
MemoryFileWriteStream >> stream [
	^ stream ifNil: [ stream := WriteStream on: file bytes from: 1 to: file size ]
]
