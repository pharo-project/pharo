"
I provide an interface for doing IO on an open file. I keep an id, which as an opaque identifier used by the FilePlugin primitives. I translate positions from the 1-based indexes used in Smalltalk to the 0-based offsets used by the primitives.

I do not implement the primitives myself, instead delegating those to an instance of FilePluginPrimitives.
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
	#category : #FileSystem-Disk
}

{ #category : #'class initialization' }
FileHandle class >> initialize [
	self useFilePlugin.

]

{ #category : #'as yet unclassified' }
FileHandle class >> register: aFileHandle [
	"properly register the given FileHandle for being closed on garbage collection"
	^self registry add: aFileHandle
]

{ #category : #'as yet unclassified' }
FileHandle class >> registry [
	^Registry ifNil: [Registry := WeakRegistry new]

]

{ #category : #'class initialization' }
FileHandle class >> startUp: resuming [
	"This functionality is disabled for now, to avoid doing a lot of processing at
	image start up. To reenable, add this class to the start up list."
	
	resuming ifTrue: [self allInstancesDo: [:ea | ea startUp]]
]

{ #category : #'class initialization' }
FileHandle class >> useFilePlugin [
	Primitives := FilePluginPrims new
]

{ #category : #public }
FileHandle >> assureOpen [
	"compatibility method to make the FileHandle Tests pass"
	self isOpen ifFalse: [ id := self basicOpen ].
]

{ #category : #public }
FileHandle >> at: index read: buffer startingAt: start count: count [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	^ Primitives 
		setPosition: id to: index - 1;
		read: id into: buffer startingAt: start count: count

]

{ #category : #public }
FileHandle >> at: index write: buffer startingAt: start count: count [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	Primitives 
		setPosition: id to: index - 1;
		write: id from: buffer startingAt: start count: count

]

{ #category : #streams-compatibility }
FileHandle >> binaryReadStream [
	^ (FileStream onHandle: self)
		ifNil: [ self streamError ]
		ifNotNil: [ :stream | 
			stream
				binary;
				yourself ]
]

{ #category : #public }
FileHandle >> close [
	Primitives close: id.
	id := nil
]

{ #category : #finalization }
FileHandle >> finalize [
	self primCloseNoError: id.
]

{ #category : #public }
FileHandle >> flush [
	
	self flag: 'TODO: remove once FileHandles are really used!'.
	self assureOpen.
	
	Primitives flush: id
]

{ #category : #testing }
FileHandle >> isOpen [
	^ (Primitives sizeOrNil: id) notNil
]

{ #category : #public }
FileHandle >> open [
	self flag: 'TODO: for now we solely rely on the old FileStreams'
	"id := self basicOpen.
	id ifNil: 	[
		reference exists ifFalse: [FileDoesNotExist signalWith: reference].
		self error: 'Unable to open file ' , reference printString]"
]

{ #category : #public }
FileHandle >> readStream [
	^ (FileStream onHandle: self)
		ifNil: [ self streamError ]
	
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
	
	^ Primitives sizeOf: id
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
	
	Primitives sync: id
]

{ #category : #public }
FileHandle >> truncateTo: anInteger [
	Primitives setPosition: id to: anInteger.
	Primitives truncate: id to: anInteger.
	self reopen
]

{ #category : #public }
FileHandle >> writeStream [
	^( FileStream onHandle: self)
		 ifNil: [ self streamError ]
]
