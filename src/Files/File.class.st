"
I represent a sequential binary File. I provide the minimum operations to:

- move the cursor fo the file
- reading
- writing

!Examples of usage

""Creating a file""
file := File named: 'asd.txt' asFileReference fullName.

""Opening / closing it""
file open.
file openForAppend.
file close.

""Accessing the file properties""
file size.
file position.
file position: 0.
file seekAbsolute: 10.
file seekRelative: 10.
file atEnd.

""Writing""
file nextPutAll: 'sdd'.

""Reading""
file next: 2.

""Buffered write""
file next: 2 putAll: 'abc' startingAt: 2.

""Buffered read""
buffer := ByteArray new: 5.
file readInto: buffer startingAt: 1 count: 5.
buffer asString.
"
Class {
	#name : #File,
	#superclass : #Object,
	#instVars : [
		'name'
	],
	#classVars : [
		'Registry'
	],
	#category : #'Files-Core'
}

{ #category : #'file creation' }
File class >> named: fileName [
	"Open a file with the given name for reading and writing. If the name has no directory part, then the file will be created in the default directory. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	^ self new named: fileName
]

{ #category : #'file creation' }
File class >> openAndTruncateForWriteFileNamed: aName [

	^ (self named: aName)
		delete;
		openForWrite
]

{ #category : #'file creation' }
File class >> openForReadFileNamed: aName [

	^ (self named: aName)
		openForRead
]

{ #category : #'file creation' }
File class >> openForWriteFileNamed: aName [

	^ (self named: aName) openForWrite
]

{ #category : #primitives }
File class >> primClose: id [
	"Close this file."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	self primitiveFailed
]

{ #category : #registry }
File class >> register: anObject [
	^self registry add: anObject
]

{ #category : #registry }
File class >> registry [
	^Registry ifNil: [Registry := WeakRegistry new]

]

{ #category : #registry }
File class >> retryWithGC: execBlock until: testBlock forFileNamed: fullName [
	"Some platforms, like Windows, will fail if we try to open a file twice.
	However, sometimes it may happen that a file was unreferenced and should be garbage-collected.
	The purpose of this method is to try to open the file, and if it fails, launch a garbage collection to try to close the unreferenced files.
	Then we will retry again to open the file"

	| blockValue foundIt |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue: [ ^ blockValue ].

	"Optimization: See if we have a file with the given name. Not really needed"
	foundIt := self registry keys "hold on strongly for now" 
		anySatisfy: [:file| file name sameAs: fullName].
	foundIt ifFalse:[^blockValue].

	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue: [ ^ blockValue ].
	Smalltalk garbageCollect.
	^execBlock value.
]

{ #category : #registry }
File class >> unregister: anObject [
	^self registry remove: anObject ifAbsent:[]
]

{ #category : #'open/close' }
File >> basicOpenForWrite: writeMode [ 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	^ self class
		retryWithGC: [ self primOpen: name writable: writeMode ]
		until:[ :id | id notNil ] 
		forFileNamed: name
]

{ #category : #'open/close' }
File >> checkDoesNotExist [
	"This method implements a backwards compatible logic to #newFileNamed:
	
	If the file does not exist, this method has no effect, and returns self.
	
	If the file exists, it will throw a FileAlreadyExistsException.
	If unhandled, this will open a pop-up asking the user to enter a new name or to override the existing file.
	In this case, this method returns a new file with the options selected by the user.
	"
	self exists ifTrue: [
		^ FileAlreadyExistsException signalOnFile: self ]
]

{ #category : #'open/close' }
File >> delete [
	"We retries with GC because in some platforms open files cannot be open"
	self class
		retryWithGC: [ self primDelete: name ]
		until: [ :result | result notNil ]
		forFileNamed: name.
	self exists
		ifTrue: [ (CannotDeleteFileException new messageText: 'Could not delete file ' , name,'. Check the file is not open.') signal ].
]

{ #category : #'open/close' }
File >> exists [

	| handle |
	"We open it for read. If the returned handle is nil, the file could not be opened"
	handle := self basicOpenForWrite: false.
	handle ifNil: [^ false].
	self class primClose: handle.
	^ true
]

{ #category : #'instance creation' }
File >> name [

	^ name
]

{ #category : #'instance creation' }
File >> named: fileName [

	name := fileName
]

{ #category : #'open/close' }
File >> openForAppend [

	| stream |
	stream := self openForWrite: true.
	^ stream setToEnd; yourself
]

{ #category : #'open/close' }
File >> openForRead [

	^ self openForWrite: false
]

{ #category : #'open/close' }
File >> openForWrite [

	^ self openForWrite: true
]

{ #category : #'open/close' }
File >> openForWrite: writeMode [
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."

	| fileHandle |
	fileHandle := self basicOpenForWrite: writeMode.
	fileHandle
		ifNil: [ "Opening the file failed.
		If the file does not exist, we throw an explicit FileDoesNotExistException.
		Otherwise, we throw a generic FileException."
			self exists
				ifFalse: [ ^ FileDoesNotExistException signalWithFile: self writeMode: writeMode ].
			CannotDeleteFileException signal: name ].
	^ (BinaryFileStream handle: fileHandle file: self forWrite: writeMode)
		register;
		yourself
]

{ #category : #primitives }
File >> primDelete: aFileName [
	"Delete the file of the given name. 
	Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil
]

{ #category : #primitives }
File >> primOpen: fileName writable: writableFlag [
	"Open a file of the given name, and return the file ID obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	^ nil

]

{ #category : #streaming }
File >> readStream [
	
	^ self openForRead
]

{ #category : #streaming }
File >> readStreamDo: aBlock [
	| stream |
	stream := self readStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
]

{ #category : #registry }
File >> unregister [
	^self class unregister: self
]

{ #category : #streaming }
File >> writeStream [

	^ self openForWrite
]

{ #category : #streaming }
File >> writeStreamDo: aBlock [
	| stream |
	stream := self writeStream.
	^ [ aBlock value: stream ]
		ensure: [ stream close ]
]
