"
I present a low-level protocol for interacting with filesystems. I hold a reference to
a store (a subinstance of FileSystemStore) which takes care of the details of performing 
file and directory operations on the filesystem I represent. 

I am responsible for resolving all paths that
I pass into my store. My store acts as a factory and offers platform specific actions.


FileSystem instances know two methods that return a Reference object: workingDirectory and root.

FileSystem disk workingDirectory
FileSystem disk root


"
Class {
	#name : 'FileSystem',
	#superclass : 'Object',
	#instVars : [
		'store'
	],
	#category : 'FileSystem-Core-Public',
	#package : 'FileSystem-Core',
	#tag : 'Public'
}

{ #category : 'system startup' }
FileSystem class >> startUp: aBoolean [
	"This is only here to deal with migration from older versions of
	FileSystem that wanted to receive startup notifcations."

	SessionManager default unregisterClassNamed: self name
]

{ #category : 'instance creation' }
FileSystem class >> store: aStore [
	^ self basicNew
		initializeWithStore: aStore;
		yourself
]

{ #category : 'navigating' }
FileSystem >> * anObject [
	"Return a relative reference."

	^ self referenceTo:( Path * anObject)
]

{ #category : 'navigating' }
FileSystem >> / anObject [
	"Return the absolute reference obtained by resolving anObject against the
	root of this filesystem."

	^ self root / anObject
]

{ #category : 'comparing' }
FileSystem >> = other [
	^ self species = other species and: [self store = other store]
]

{ #category : 'public' }
FileSystem >> accessTimeOf: aResolvable [
	"Returns the access date of aResolvable"

	^ store accessTimeOf: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> beReadOnly [

	store := store asReadOnlyStore
]

{ #category : 'public' }
FileSystem >> beWritable [

	store := store asWritableStore
]

{ #category : 'public' }
FileSystem >> binaryReadStreamOn: aResolvable [
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a read stream for reading the file."

	^ (self open: aResolvable writable: false) binaryReadStream
]

{ #category : 'public' }
FileSystem >> binaryWriteStreamOn: aResolvable [
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a binary write stream for reading the file."

	^ (self open: aResolvable writable: true) binaryWriteStream
]

{ #category : 'public' }
FileSystem >> changeTimeOf: aResolvable [
	"Returns the change time of aResolvable"

	^ store changeTimeOf: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> checkName: aString fixErrors: fixErrors [
	^ store checkName: aString fixErrors: fixErrors
]

{ #category : 'public-enumerating' }
FileSystem >> childNamesAt: aResolvable [
	^ Array streamContents: [ :out |
		self childNamesAt: aResolvable do: [ :path|
			out nextPut: path ]]
]

{ #category : 'public-enumerating' }
FileSystem >> childNamesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		nodesDo: [ :entry |
			aBlock value: (store basenameFromEntry: entry) ]
]

{ #category : 'public-enumerating' }
FileSystem >> childrenAt: aResolvable [
	^ Array streamContents: [ :out |
		self childrenAt: aResolvable do: [ :path|
			out nextPut: path ]]
]

{ #category : 'public-enumerating' }
FileSystem >> childrenAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		nodesDo: [ :entry |
			aBlock value: path / (store basenameFromEntry: entry) ]
]

{ #category : 'public' }
FileSystem >> close [
	store close
]

{ #category : 'public' }
FileSystem >> copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock [
	"Copy the file referenced as sourcePath to the destination referred as destPath. Perform associate actions in case of problems."

	store
		copy: (self resolve: sourcePath)
		ifAbsent: absentBlock
		to: (self resolve: destinationPath)
		ifPresent: presentBlock
		fileSystem: self
]

{ #category : 'public' }
FileSystem >> copy: sourcePath to: destPath [
	"Copy the file referenced as sourcePath to the destination referred as destPath.
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists."

	self
		copy: sourcePath
		ifAbsent: [ store signalFileDoesNotExist: sourcePath ]
		to: destPath
		ifPresent: [ store signalFileExists: destPath ]
]

{ #category : 'public' }
FileSystem >> copy: aPath toReference: destinationReference [

	^self = destinationReference fileSystem
		ifTrue: [ self copy: aPath to: destinationReference resolve path ]
		ifFalse: [ self copy: aPath toRemote: destinationReference resolve ]
]

{ #category : 'public' }
FileSystem >> copy: aPath toRemote: destRef [
	| inputStream path |
	path := self resolve: aPath.
	[ inputStream := self binaryReadStreamOn: path.
	inputStream ifNil: [ store signalFileDoesNotExist: path ].
	destRef fileSystem copyFrom: inputStream to: destRef path ]
		ensure: [ inputStream ifNotNil: [ inputStream close ] ]
]

{ #category : 'public' }
FileSystem >> copyAndDelete: sourcePath to: destination [
	"Copy the file referenced as sourcePath to the destination referred as destPath.
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If an error occurs during the operation, try and roll back to the original state."

	^(self isDirectory: sourcePath) ifTrue:
		[ self copyDirectoryAndDelete: sourcePath to: destination ]
	ifFalse:
		[ self copyFileAndDelete: sourcePath to: destination ]
]

{ #category : 'private' }
FileSystem >> copyDirectory: sourcePath toReference: destination [
	"Copy the source directory to the (yet to be created) destination"

	destination createDirectory.
	store directoryAt: sourcePath nodesDo:
		[ :entry | | basename |
			basename := store basenameFromEntry: entry.
			(store basicIsDirectory: entry) ifTrue:
				[ self copyDirectory: sourcePath / basename toReference: destination / basename ]
			ifFalse:
				[ self copy: sourcePath / basename toReference: destination / basename ] ]
]

{ #category : 'private' }
FileSystem >> copyDirectoryAndDelete: sourcePath to: destination [
	"Copy the directory sourcePath to the destination referred as destPath.
	If destination exists (and is a directory), add sourcePath basename to it,
	otherwise create destination as a directory a copy the contents of sourcePath in to it.
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If an error occurs during the operation, try and roll back to the original state."

	"The source must be a directory"
	(self isDirectory: sourcePath) ifFalse:
		[ (FileException fileName: sourcePath pathString)
				message: 'not a directory';
				signal ].
	"The destination musn't exist yet"
	destination ifExists: [ FileAlreadyExistsException signalWith: destination ].

	[self copyDirectory: sourcePath toReference: destination]
		on: Error
		do: [ :error |
			destination delete.
			error signal].
	(FileReference fileSystem: self path: sourcePath) deleteAll.
	^destination
]

{ #category : 'private' }
FileSystem >> copyFileAndDelete: sourcePath to: destination [
	"Copy the file referenced as sourcePath to the destination referred as destPath.
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If an error occurs during the operation, try and roll back to the original state."

	[self copy: sourcePath toReference: destination]
		on: Error
		do: [ :error |
			destination ensureDelete.
			error signal].
	self delete: sourcePath.
	^destination
]

{ #category : 'private' }
FileSystem >> copyFrom: inputStream to: destPath [
	| buffer out |
	out := nil.
	(self exists: destPath)
		ifTrue: [ store signalFileExists: destPath ].
	^ [
	out := self binaryWriteStreamOn: destPath.
	buffer := ByteArray new: 1024.
	[ inputStream atEnd ]
		whileFalse: [
			buffer := inputStream nextInto: buffer.
			out nextPutAll: buffer ]] ensure: [ out ifNotNil: [ out close ]]
]

{ #category : 'public' }
FileSystem >> createDirectory: aResolvable [
	"Resolve aResolvable into an absolute path, then as the store to create a directory there.
	The store is expected to raise an exception if it cannot do so."

	^ store createDirectory: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> creationTimeOf: aResolvable [
	"Returns the creation date of aResolvable"

	^ store creationTimeOf: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> delete: aResolvable [
	store delete: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> delimiter [
	"Return path delimiter used by this filesystem."

	^ store delimiter
]

{ #category : 'public' }
FileSystem >> deviceIdOf: aResolvable [
	"Returns the device id of aResolvable"

	^ store deviceIdOf: (self resolve: aResolvable)
]

{ #category : 'public-enumerating' }
FileSystem >> directoriesAt: aResolvable [
	^ Array streamContents: [ :out |
		self directoriesAt: aResolvable do: [ :path|
			out nextPut: path ]]
]

{ #category : 'public-enumerating' }
FileSystem >> directoriesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		directoryNodesDo: [ :entry |
			aBlock value: path / (store basenameFromEntry: entry) ]
]

{ #category : 'public-enumerating' }
FileSystem >> directoryNamesAt: aResolvable [
	^ Array streamContents: [ :out |
		self directoryNamesAt: aResolvable do: [ :name|
			out nextPut: name ]]
]

{ #category : 'public-enumerating' }
FileSystem >> directoryNamesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		directoryNodesDo: [ :node |
			aBlock value: (store basenameFromEntry: node) ]
]

{ #category : 'public' }
FileSystem >> ensureCreateDirectory: aResolvable [
	"Resolve the argument to an absolute path, then ask the store to make
	sure that all the directories contained in the argument path exist or are created."

	store ensureCreateDirectory: (self resolve: aResolvable)
]

{ #category : 'public-enumerating' }
FileSystem >> entriesAt: aResolvable [
	^ Array
		streamContents: [ :out |
			self entriesAt: aResolvable do: [ :entry | out nextPut: entry ]
		]
]

{ #category : 'public-enumerating' }
FileSystem >> entriesAt: aResolvable do: aBlock [
	| path entry |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		nodesDo: [ :node |
			entry := store entryFromNode: node path: path for: self.
			aBlock value: entry ]
]

{ #category : 'public-enumerating' }
FileSystem >> entriesAt: aResolvable ifAbsent: absentBlock do: aBlock [
	| path entry aFilesystem |
	path := self resolve: aResolvable.
	aFilesystem := self.
	store
		directoryAt: path
		ifAbsent: [ ^ absentBlock value ]
		nodesDo: [ :node |
			entry := store entryFromNode: node path: path for: aFilesystem.
			aBlock value: entry ]
]

{ #category : 'public' }
FileSystem >> entryAt: aResolvable [
	"Answer the FileSystemDirectoryEntry for aResolvable"

	^ store entryAt: (self resolve: aResolvable) fileSystem: self
]

{ #category : 'public' }
FileSystem >> exists: aResolvable [
	"Resolve the argument, and answer true if the there is
	a file or directory at that path, false if there is not."

	^ store exists: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> extensionDelimiter [
	^ $.
]

{ #category : 'public' }
FileSystem >> file: aResolvable posixPermissions: anInteger [
	"Set the mode of aResolvable to anInteger (as defined by chmod())"

	^store file: (self resolve: aResolvable) posixPermissions: anInteger
]

{ #category : 'public' }
FileSystem >> file: aResolvable symlinkUid: uidInteger gid: gidInteger [
	"Set the owner and group of aResolvable by numeric id."

	^store file: (self resolve: aResolvable) symlinkUid: uidInteger gid: gidInteger
]

{ #category : 'public' }
FileSystem >> file: aResolvable uid: uidInteger gid: gidInteger [
	"Set the owner and group of aResolvable by numeric id."

	^store file: (self resolve: aResolvable) uid: uidInteger gid: gidInteger
]

{ #category : 'public-enumerating' }
FileSystem >> fileNamesAt: aResolvable [
	^ Array streamContents: [ :out |
		self fileNamesAt: aResolvable do: [ :path|
			out nextPut: path ]]
]

{ #category : 'public-enumerating' }
FileSystem >> fileNamesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		fileNodesDo: [ :entry |
			aBlock value: (store basenameFromEntry: entry) ]
]

{ #category : 'public-enumerating' }
FileSystem >> filesAt: aResolvable [
	^ Array streamContents: [ :out |
		self filesAt: aResolvable do: [ :path|
			out nextPut: path ]]
]

{ #category : 'public-enumerating' }
FileSystem >> filesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		fileNodesDo: [ :entry |
			aBlock value: path / (store basenameFromEntry: entry) ]
]

{ #category : 'printing' }
FileSystem >> forReferencePrintOn: aStream [
	store forReferencePrintOn: aStream
]

{ #category : 'public' }
FileSystem >> gidOf: aResolvable [
	"Returns the group id of aResolvable"

	^ store gidOf: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> hasChildren: aResolvable [
	"Returns whether aResolvable has children."
	store directoryAt: (self resolve: aResolvable)
			ifAbsent: [ ^false ]
			nodesDo:  [ :node | ^true ].
	^false
]

{ #category : 'public-testing' }
FileSystem >> hasDirectories: aResolvable [
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry |
			entry isDirectory ifTrue: [ ^true ] ].
	^false
]

{ #category : 'public-testing' }
FileSystem >> hasFiles: aResolvable [
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry | entry isFile ifTrue: [ ^true ] ].
	^false
]

{ #category : 'comparing' }
FileSystem >> hash [
	^ store hash
]

{ #category : 'initialization' }
FileSystem >> initializeWithStore: aStore [
	store := aStore
]

{ #category : 'public' }
FileSystem >> inodeOf: aResolvable [
	"Returns the inode number of aResolvable"

	^ store inodeOf: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isBlock: aResolvable [
	"Return a boolean indicating whether the File described by aPath is a block device"

	^ store isBlock: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isCaseSensitive [
	^ self store isCaseSensitive
]

{ #category : 'public-testing' }
FileSystem >> isCharacter: aResolvable [
	"Return a boolean indicating whether the File described by aPath is character based"

	^ store isCharacter: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isDirectory: aResolvable [
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store isDirectory: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isExecutable: aResolvable [
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."

	^ store isExecutable: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isFIFO: aResolvable [
	"Return a boolean indicating whether the File described by aPath is FIFO"

	^ store isFIFO: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isFile: aResolvable [
	"Resolve the argument, and answer true if the result refers
	to a file, false if it refers to a directory or doesn't exist."

	^ store isFile: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isHidden: aResolvable attributes: statAttributesArray [
	"Resolve the argument, and answer a boolean indicating whether the file is hidden or not.
	statAttributesArray is optional and may be supplied for performance.  It is the result of File>>fileAttributes:mask: or nil"

	^ store isHidden: (self resolve: aResolvable) attributes: statAttributesArray
]

{ #category : 'public-testing' }
FileSystem >> isReadable: aResolvable [
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."

	^ store isReadable: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isRegular: aResolvable [
	"Return a boolean indicating whether the File described by aPath is a regular file"

	^ store isRegular: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isSocket: aResolvable [
	"Return a boolean indicating whether the File described by aPath is a regular file"

	^ store isSocket: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isSymlink: aResolvable [
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store isSymlink: (self resolve: aResolvable)
]

{ #category : 'public-testing' }
FileSystem >> isWritable [

	^ self store isWritable
]

{ #category : 'public-testing' }
FileSystem >> isWritable: aResolvable [
	"Resolve the argument, and answer true if the there is
	a file that can be written to or directory that can be changed."

	^ store isWritable: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> modificationTimeOf: aResolvable [
	"Returns the last date of modification of aResolvable"

	^ store modificationTimeOf: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> move: sourcePath to: destination [
	"Move the file /directory referenced as sourcePath to the destination referred as destPath.
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If destPath is a directory, move the sourcePath in to the directory"

	| fullDestination destinationPath |

	destination isFile ifTrue: [ FileExists signalWith: destination ].
	destination isDirectory
		ifTrue: [ fullDestination := destination / sourcePath basename ]
		ifFalse: [ fullDestination := destination ].
	self = destination fileSystem ifTrue:
	[
		destinationPath := fullDestination resolve path.
		sourcePath = destinationPath ifTrue: [ ^fullDestination ].
		"Ideally we would test whether the source and destination are on the same filesystem from the OSs perspective.
		Since we can't do that, just try rename, and if that fails, copy and delete."
		[ self rename: sourcePath to: destinationPath ]
			on: Error
			do: [ :error | self copyAndDelete: sourcePath to: fullDestination ].
	] ifFalse:
		[ self copyAndDelete: sourcePath to: fullDestination ].
	^fullDestination
]

{ #category : 'public-testing' }
FileSystem >> numberOfHardLinks: aResolvable [
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store numberOfHardLinks: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> open [
	"Some kinds of filesystems need to open connections to external resources. Does nothing by default."

	store open
]

{ #category : 'public' }
FileSystem >> open: aResolvable writable: aBoolean [
	"Resolve aResolvable into an absolute path, then ask the store to open the file at
	that path using the specified access mode."

	| path |
	path := self resolve: aResolvable.
	^ store handleClass
		open: (FileReference fileSystem: self path: path)
		writable: aBoolean
]

{ #category : 'private' }
FileSystem >> openStreamDescription: aResolvable writable: aBoolean [
	"I am  a helper method to delegate basicOpen:writable: to the store.
	 I am called from FileSystemHandle implementations."

	| path |

	path := self resolve: aResolvable.
	^ store basicOpen: path writable: aBoolean
]

{ #category : 'converting' }
FileSystem >> pathFromObject: anObject [
	^ anObject asPathWith: self
]

{ #category : 'converting' }
FileSystem >> pathFromString: aString [
	^ store pathFromString: aString
]

{ #category : 'public' }
FileSystem >> permissions: aResolvable [
	"Resolve the argument and return the Permissions for this file or directory "

	^ store permissions: (self resolve: aResolvable)
]

{ #category : 'printing' }
FileSystem >> printPath: aPath on: aStream [
	store printPath: aPath on: aStream
]

{ #category : 'public' }
FileSystem >> readStreamOn: aResolvable [
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a read stream for reading the file."

	^ (self open: aResolvable writable: false) readStream
]

{ #category : 'public' }
FileSystem >> referenceTo: aResolvable [
	"Answer a reference to the argument from the context of the receiver filesystem."

	" (FileSystem disk referenceTo: 'plonk.taz') class >>> FileReference "
	
	^ FileReference
		fileSystem: self
		path: (self pathFromObject: aResolvable)
]

{ #category : 'public' }
FileSystem >> rename: sourcePath ifAbsent: aBlock to: destPath ifPresent: pBlock [
	"Rename the file referenced as sourcePath to the destination referred as destPath.
	Perform associate actions in case of problems."

	| source destination |
	source := self resolve: sourcePath.
	destination := self resolve: destPath.
	store
		rename: source
		ifAbsent: aBlock
		to: destination
		ifPresent: pBlock
		fileSystem: self.
	^ destination
]

{ #category : 'public' }
FileSystem >> rename: sourcePath to: destName [
	"Rename the file referenced as sourcePath to destPath.  Raise exceptions
	FileExists or FileDoesNotExist  if the operation fails"

	^ self
		rename: sourcePath
		ifAbsent: [store signalFileDoesNotExist: sourcePath]
		to: destName
		ifPresent: [store signalFileExists: destName]
]

{ #category : 'public' }
FileSystem >> resolve: aResolvable [
	^ aResolvable asResolvedBy: self
]

{ #category : 'navigating' }
FileSystem >> resolvePath: aPath [
	"Return a path where the argument is resolved in the context of the
	receiver. The behavior is similar to the one of a command line.
		> cd /a/b/c
		> cd b
		The shell will attempt to make /a/b/c/b the current directory. "

	^ self workingDirectoryPath resolve: aPath
]

{ #category : 'navigating' }
FileSystem >> resolveReference: aReference [
	^ aReference fileSystem = self ifTrue:
		[self workingDirectoryPath resolvePath: aReference path]
]

{ #category : 'navigating' }
FileSystem >> resolveString: aString [
	"Returns the root of the receiver filesystem, i.e. / on unix"

	^ self workingDirectoryPath resolvePath: (store pathFromString: aString)
]

{ #category : 'accessing' }
FileSystem >> root [
	"Returns the root of the receiver filesystem, i.e. / on unix"
	^ self referenceTo: Path root
]

{ #category : 'accessing' }
FileSystem >> separator [
	"Return path separator used by this filesystem."

	^ store separator
]

{ #category : 'public' }
FileSystem >> sizeOf: aResolvable [
	"Resolve the argument and return the size for this file or directory "

	^ store sizeOf: (self resolve: aResolvable)
]

{ #category : 'accessing' }
FileSystem >> store [
	^ store
]

{ #category : 'accessing' }
FileSystem >> store: aFileSystemStore [
	store := aFileSystemStore
]

{ #category : 'converting' }
FileSystem >> stringFromPath: aPath [
	^ store stringFromPath: aPath
]

{ #category : 'public' }
FileSystem >> symlinkEntryAt: aResolvable [
	"Answer the DiskSymlinkDirectoryEntry for aResolvable"

	^ store symlinkEntryAt: (self resolve: aResolvable) fileSystem: self
]

{ #category : 'public-testing' }
FileSystem >> targetPath: aResolvable [
	"Return the target file of the File described by aPath.  For a regular file, this is itself, for a symbolic link, it is the file pointed to by the symbolic link"

	^ store targetPath: (self resolve: aResolvable)
]

{ #category : 'public' }
FileSystem >> uidOf: aResolvable [
	"Returns the user id of aResolvable"

	^ store uidOf: (self resolve: aResolvable)
]

{ #category : 'accessing' }
FileSystem >> workingDirectory [
	"Returns a reference to the directory from where the image was launched"

	^ self referenceTo: self workingDirectoryPath
]

{ #category : 'accessing' }
FileSystem >> workingDirectoryPath [
	^ store defaultWorkingDirectory
]

{ #category : 'public' }
FileSystem >> writeStreamOn: aResolvable [
	"Open a write stream on the file referred by the argument. It can be a string or a path"

	^ (self open: aResolvable writable: true) writeStream
]
