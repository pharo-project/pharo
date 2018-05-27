"
I represent a sequential binary File. I provide the minimum operations to:

- move the cursor fo the file
- reading
- writing

I am also the entry point of the FilePlugin primitives.

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

{ #category : #'primitives-file' }
File class >> atEnd: id [
	"Answer true if the file position is at the end of the file."

	<primitive: 'primitiveFileAtEnd' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'primitives-file' }
File class >> close: id [
	"Close this file."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	
]

{ #category : #'primitives-file' }
File class >> closed: id [
	^ id isNil or: [ (self sizeOrNil: id) isNil ]
]

{ #category : #'primitives-file' }
File class >> connectToFile: filePointer writable: writableFlag [
	"Open the file with the supplied FILE* pointer, and return the file ID obtained.
	writeableFlag indicates whether to allow write operations and must be compatible with the way the file was opened.
	It is the responsibility of the caller to coordinate closing the file."

	<primitive: 'primitiveConnectToFile' module: 'FilePlugin' error: error>
	error = #'bad argument' ifTrue: [ 
		(filePointer isKindOf: ByteArray) ifFalse: 
			[ ^self error: 'filePointer must be a ByteArray' ].
		(writableFlag isKindOf: Boolean) ifFalse: 
			[ ^self error: 'writableFlag must be a boolean' ] ].
	^ self primitiveFailed 

]

{ #category : #'primitives-file' }
File class >> connectToFileDescriptor: fileDescriptor writable: writableFlag [
	"Connect to the file with fileDescriptor number, and return the file ID obtained.
	writeableFlag indicates whether to allow write operations and must be compatible with the way the file was opened.
	It is the responsibility of the caller to coordinate closing the file."

	<primitive: 'primitiveConnectToFileDescriptor' module: 'FilePlugin' error: error>
	error = #'bad argument' ifTrue: [ 
		fileDescriptor isInteger ifFalse: 
			[ ^self error: 'fileDescriptor must be an integer' ].
		(writableFlag isKindOf: Boolean) ifFalse: 
			[ ^self error: 'writableFlag must be a boolean' ] ].
	^ self primitiveFailed 

]

{ #category : #'primitives-path' }
File class >> createDirectory: fullPath [
	"Create a directory named by the given path. 
	Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	^ nil

]

{ #category : #'primitives-encoding' }
File class >> decodePathString: aString [

	^ (ZnCharacterEncoder newForEncoding: 'utf8') decodeBytes: aString asByteArray
]

{ #category : #'primitives-path' }
File class >> deleteDirectory: fullPath [
	"Delete the directory named by the given path. 
	Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'primitives-path' }
File class >> deleteFile: aFileName [
	"Delete the file of the given name. 
	Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil

]

{ #category : #'primitives-path' }
File class >> delimiter [
	"Return the path delimiter for the underlying platform's file system."

 	<primitive: 'primitiveDirectoryDelimitor' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'primitives-encoding' }
File class >> encodePathString: aString [

	"Should not return a string"
	^ ((ZnCharacterEncoder newForEncoding: 'utf8') encodeString: aString) asString
]

{ #category : #'primitives-file' }
File class >> fileDescriptorType: fdNum [
	"Allow to test if the standard input/output files are from a console or not
	Return values:
	* -1 - Error
	* 0 - no console (windows only)
	* 1 - normal terminal (unix terminal / windows console)
	* 2 - pipe
	* 3 - file
	* 4 - cygwin terminal (windows only)"

	<primitive: 'primitiveFileDescriptorType' module: 'FilePlugin' error: error>
	error = #'bad argument'
		ifTrue: [ fdNum isInteger
				ifFalse: [ ^ self error: 'File Descriptor must be an integer!' ] ].
	^ self primitiveFailed
]

{ #category : #'primitives-file' }
File class >> flush: id [
	"On Unix, the FilePlugin uses stdio FILE* structs which maintain their
	own internal buffer to minimize write() syscalls. This flushes that buffer.
	On Windows this and primSync: do the same thing."

	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
	
	"We can't ignore fflush() failing, because it can fail for any of the
	reasons write() can."
	self primitiveFailed
]

{ #category : #'primitives-path' }
File class >> getMacFile: fileName type: typeString creator: creatorString [
	"Get the Macintosh file type and creator info for the file with the given name. Fails if the file does not exist or if the type and creator type arguments are not strings of length 4. This primitive is Mac specific; it is a noop on other platforms."

 	<primitive: 'primitiveDirectoryGetMacTypeAndCreator' module: 'FilePlugin'>


]

{ #category : #'primitives-file' }
File class >> getPosition: id [
	"Get this files current position."

	<primitive: 'primitiveFileGetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'primitives-path' }
File class >> lookupDirectory: fullPath filename: fileName [

	"Look up <fileName> (a simple file name) in the directory identified by <fullPath> and return an array containing:

	<fileName> 			
	<creationTime> 			(in seconds since the start of the Smalltalk time epoch)
	<modificationTime> 	(in seconds since the start of the Smalltalk time epoch)
	<dirFlag> 				DirFlag is true if the entry is a directory
	<fileSize> 				FileSize the file size in bytes or zero for directories
	<posixPermissions> 	Numeric Notation 
	<symLinkFlag>			seemingly, symLinkFlag is true if the entry is a symLink

	On Unix, the empty path denotes '/'. 
   On Macs and PCs, it is the container of the system volumes."

 	<primitive: 'primitiveDirectoryEntry' module: 'FilePlugin'>
	
	^ #badDirectoryPath


]

{ #category : #'primitives-path' }
File class >> lookupEntryIn: fullPath index: index [
	"Look up the index-th entry of the directory with the given fully-qualified path 
	(i.e., starting from the root of the file hierarchy) and return an array containing:

	<name> <creationTime> <modificationTime> <dirFlag> <fileSize>

	The empty string enumerates the top-level files or drives. (For example, on Unix, the empty 
	path enumerates the contents of '/'. On Macs and PCs, it enumerates the mounted volumes/drives.)

	The creation and modification times are in seconds since the start of the Smalltalk time epoch. 
	DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories. 
	The primitive returns nil when index is past the end of the directory. It fails if the given path 
	is bad."

 	<primitive: 'primitiveDirectoryLookup' module: 'FilePlugin' error: errorCode >
	
	^ #badDirectoryPath


]

{ #category : #'file creation' }
File class >> named: fileName [
	"Open a file with the given name for reading and writing. If the name has no directory part, then the file will be created in the default directory. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close."

	^ self new named: fileName
]

{ #category : #'primitives-file' }
File class >> open: fileName writable: writableFlag [
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

{ #category : #'primitives-file' }
File class >> read: id into: byteArray startingAt: startIndex count: count [
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	(self closed: id)
		ifTrue: [ ^ self error: 'File is closed' ].
	self error: 'File read failed'
]

{ #category : #registry }
File class >> register: anObject [
	^self registry add: anObject
]

{ #category : #registry }
File class >> registry [
	^Registry ifNil: [Registry := WeakRegistry new]

]

{ #category : #'primitives-path' }
File class >> rename: oldFileFullName to: newFileFullName [ 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name 
	or if there is an existing file with the new name."

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil
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

{ #category : #'primitives-file' }
File class >> setPosition: id to: anInteger [
	"Set this file to the given position."

	<primitive: 'primitiveFileSetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'primitives-file' }
File class >> sizeOf: id [
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'primitives-file' }
File class >> sizeOrNil: id [
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	^ nil
]

{ #category : #'primitives-file' }
File class >> stdioDescriptorIsATTY [
	^ (0 to: 2)
		anySatisfy: [ :fdNum | 
			| res |
			res := self fileDescriptorType: fdNum.
			#(1 4) includes: res ]
]

{ #category : #'primitives-file' }
File class >> stdioHandles [
	<primitive: 'primitiveFileStdioHandles' module: 'FilePlugin' error: ec>
	self primitiveFailed
]

{ #category : #'primitives-file' }
File class >> sync: id [
	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing"

	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

	"fsync() failing cannot be ignored"
	self primitiveFailed
]

{ #category : #'primitives-file' }
File class >> truncate: id to: anInteger [
	"Truncate this file to the given position."

	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #registry }
File class >> unregister: anObject [
	^self registry remove: anObject ifAbsent:[]
]

{ #category : #'primitives-file' }
File class >> write: id from: stringOrByteArray startingAt: startIndex count: count [
	"Write count bytes onto this file from the given string or byte array starting at the given index. 	Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	self primitiveFailed 
]

{ #category : #'open/close' }
File >> basicOpenForWrite: writeMode [ 
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	^ self class
		retryWithGC: [ self class open: name utf8Encoded writable: writeMode ]
		until:[ :id | id notNil ] 
		forFileNamed: name
]

{ #category : #testing }
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
		retryWithGC: [ self class deleteFile: name utf8Encoded ]
		until: [ :result | result notNil ]
		forFileNamed: name.
	self exists
		ifTrue: [ (CannotDeleteFileException new messageText: 'Could not delete file ' , name,'. Check the file is not open.') signal ].
]

{ #category : #testing }
File >> exists [

	| handle |
	"We open it for read. If the returned handle is nil, the file could not be opened"
	handle := self basicOpenForWrite: false.
	handle ifNil: [^ false].
	self class close: handle.
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
	fileHandle ifNil: [
		"Opening the file failed.
		If the file does not exist, we throw an explicit FileDoesNotExistException.
		Otherwise, we throw a generic FileException."
		self exists
			ifFalse: [ ^ FileDoesNotExistException signalWithFile: self writeMode: writeMode ].
		CannotDeleteFileException signal: name
	].

	^ (BinaryFileStream handle: fileHandle file: self forWrite: writeMode)
		register;
		yourself
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

{ #category : #accessing }
File >> size [

	^ self readStreamDo: [ :stream | stream size ]
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
