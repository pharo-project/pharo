"
I provide an interface to the primitives in the FilePlugin.

"
Class {
	#name : #FilePluginPrims,
	#superclass : #Object,
	#category : #'FileSystem-Disk-Base'
}

{ #category : #'file primitives' }
FilePluginPrims >> atEnd: id [
	"Answer true if the file position is at the end of the file."

	<primitive: 'primitiveFileAtEnd' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'file primitives' }
FilePluginPrims >> close: id [
	"Close this file."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	
]

{ #category : #'file primitives' }
FilePluginPrims >> connectToFile: filePointer writable: writableFlag [
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

{ #category : #'file primitives' }
FilePluginPrims >> connectToFileDescriptor: fileDescriptor writable: writableFlag [
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

{ #category : #'path primitives' }
FilePluginPrims >> createDirectory: fullPath [
	"Create a directory named by the given path. 
	Fail if the path is bad or if a file or directory by that name already exists."

 	<primitive: 'primitiveDirectoryCreate' module: 'FilePlugin'>
	^ nil

]

{ #category : #'encoding primitives' }
FilePluginPrims >> decode: aString [

	^ (ZnCharacterEncoder newForEncoding: 'utf8') decodeBytes: aString asByteArray
]

{ #category : #'path primitives' }
FilePluginPrims >> deleteDirectory: fullPath [
	"Delete the directory named by the given path. 
	Fail if the path is bad or if a directory by that name does not exist."

 	<primitive: 'primitiveDirectoryDelete' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'path primitives' }
FilePluginPrims >> deleteFile: aFileName [
	"Delete the file of the given name. 
	Return self if the primitive succeeds, nil otherwise."

	<primitive: 'primitiveFileDelete' module: 'FilePlugin'>
	^ nil

]

{ #category : #'path primitives' }
FilePluginPrims >> delimiter [
	"Return the path delimiter for the underlying platform's file system."

 	<primitive: 'primitiveDirectoryDelimitor' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'encoding primitives' }
FilePluginPrims >> encode: aString [

	"Should not return a string"
	^ ((ZnCharacterEncoder newForEncoding: 'utf8') encodeString: aString) asString
]

{ #category : #'file primitives' }
FilePluginPrims >> flush: id [
	"On Unix, the FilePlugin uses stdio FILE* structs which maintain their
	own internal buffer to minimize write() syscalls. This flushes that buffer.
	On Windows this and primSync: do the same thing."

	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
	
	"We can't ignore fflush() failing, because it can fail for any of the
	reasons write() can."
	self primitiveFailed
]

{ #category : #'path primitives' }
FilePluginPrims >> getMacFile: fileName type: typeString creator: creatorString [
	"Get the Macintosh file type and creator info for the file with the given name. Fails if the file does not exist or if the type and creator type arguments are not strings of length 4. This primitive is Mac specific; it is a noop on other platforms."

 	<primitive: 'primitiveDirectoryGetMacTypeAndCreator' module: 'FilePlugin'>


]

{ #category : #'file primitives' }
FilePluginPrims >> getPosition: id [
	"Get this files current position."

	<primitive: 'primitiveFileGetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'path primitives' }
FilePluginPrims >> imageFile [
	"Answer the full path name for the current image."

	<primitive: 121>
	self primitiveFailed
]

{ #category : #'path primitives' }
FilePluginPrims >> lookupDirectory: fullPath filename: fileName [

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

{ #category : #'path primitives' }
FilePluginPrims >> lookupEntryIn: fullPath index: index [
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

{ #category : #'file primitives' }
FilePluginPrims >> open: fileName writable: writableFlag [
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

{ #category : #'file primitives' }
FilePluginPrims >> read: id into: byteArray startingAt: startIndex count: count [
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self primitiveFailed
]

{ #category : #'path primitives' }
FilePluginPrims >> rename: oldFileFullName to: newFileFullName [ 
	"Rename the file of the given name to the new name. Fail if there is no file of the old name 
	or if there is an existing file with the new name."

	<primitive: 'primitiveFileRename' module: 'FilePlugin'>
	^nil
]

{ #category : #'path primitives' }
FilePluginPrims >> setMacFileNamed: fileName type: typeString creator: creatorString [
	"Set the Macintosh file type and creator info for the file with the given name. Fails if the 
	file does not exist or if the type and creator type arguments are not strings of length 4. This
	primitive is Mac specific; it is a noop on other platforms."

 	<primitive: 'primitiveDirectorySetMacTypeAndCreator' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'file primitives' }
FilePluginPrims >> setPosition: id to: anInteger [
	"Set this file to the given position."

	<primitive: 'primitiveFileSetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'file primitives' }
FilePluginPrims >> sizeOf: id [
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'file primitives' }
FilePluginPrims >> sizeOrNil: id [
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	^ nil
]

{ #category : #'file primitives' }
FilePluginPrims >> sync: id [
	"On Unix, this syncs any written or flushed data still in the kernel file
	system buffers to disk. On Windows this and primFlush: do the same thing"

	<primitive: 'primitiveFileSync' module: 'FilePlugin'>

	"fsync() failing cannot be ignored"
	self primitiveFailed
]

{ #category : #'file primitives' }
FilePluginPrims >> truncate: id to: anInteger [
	"Truncate this file to the given position."

	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : #'file primitives' }
FilePluginPrims >> write: id from: stringOrByteArray startingAt: startIndex count: count [
	"Write count bytes onto this file from the given string or byte array starting at the given index. 	Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	self primitiveFailed
]
