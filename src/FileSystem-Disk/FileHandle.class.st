"
I provide an interface for doing IO on an open file. I keep an id, which as an opaque identifier used by the FilePlugin primitives. I translate positions from the 1-based indexes used in Smalltalk to the 0-based offsets used by the primitives.

I implement the primitives on my class side.
"
Class {
	#name : #FileHandle,
	#superclass : #FileSystemHandle,
	#instVars : [
		'id'
	],
	#classVars : [
		'Registry'
	],
	#category : #'FileSystem-Disk-Base'
}

{ #category : #registry }
FileHandle class >> register: aFileHandle [
	"properly register the given FileHandle for being closed on garbage collection"
	^self registry add: aFileHandle
]

{ #category : #registry }
FileHandle class >> registry [
	^Registry ifNil: [Registry := WeakRegistry new]

]

{ #category : #'class initialization' }
FileHandle class >> startUp: resuming [
	"This functionality is disabled for now, to avoid doing a lot of processing at
	image start up. To reenable, add this class to the start up list."
	
	resuming ifTrue: [self allInstancesDo: [:ea | ea startUp]]
]

{ #category : #public }
FileHandle >> assureOpen [
	"Compatibility method to make the FileHandle Tests pass"
	self isOpen ifFalse: [ id := self basicOpen ].
]

{ #category : #public }
FileHandle >> at: index read: buffer startingAt: start count: count [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ File 
		setPosition: id to: index - 1;
		read: id into: buffer startingAt: start count: count

]

{ #category : #public }
FileHandle >> at: index write: buffer startingAt: start count: count [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	File 
		setPosition: id to: index - 1;
		write: id from: buffer startingAt: start count: count

]

{ #category : #public }
FileHandle >> binaryReadStream [
	^ ZnBufferedReadStream on: (File named: reference fullName) readStream
]

{ #category : #public }
FileHandle >> binaryWriteStream [
	^ (File named: reference fullName) writeStream
]

{ #category : #public }
FileHandle >> close [
	File close: id.
	id := nil
]

{ #category : #finalization }
FileHandle >> finalize [
	File close: id
]

{ #category : #public }
FileHandle >> flush [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	File flush: id
]

{ #category : #testing }
FileHandle >> isOpen [
	^ (File sizeOrNil: id) notNil
]

{ #category : #public }
FileHandle >> open [
	self flag: 'TODO: for now we solely rely on the old FileStreams'
	"id := self basicOpen.
	id ifNil: 	[
		reference exists ifFalse: [FileDoesNotExist signalWith: reference].
		self error: 'Unable to open file ' , reference printString]"
]

{ #category : #finalization }
FileHandle >> register [
	"register the instance for proper clreanup on garbage collection"
	^self class register: self
]

{ #category : #public }
FileHandle >> size [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ File sizeOf: id
]

{ #category : #private }
FileHandle >> startUp [
	"This functionality is disabled for now, to avoid doing lots of processing
	on start up."
	
	"We're starting up in a new OS process, so the file id will be invalid.
	Try to reopen the file, but fail silently: just leave the id as nil. #isOpen will
	answer false, and we'll raise an error if anyone tries to do IO."
	
	self basicOpen
]

{ #category : #public }
FileHandle >> streamError [
	reference exists
		ifFalse: [FileDoesNotExist signalWith: reference].
	self error: 'Unable to open file ' , reference printString
]

{ #category : #public }
FileHandle >> sync [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	File sync: id
]

{ #category : #public }
FileHandle >> truncateTo: anInteger [
	File setPosition: id to: anInteger.
	File truncate: id to: anInteger.
	self reopen
]
