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
	#name : #FileSystem,
	#superclass : #Object,
	#instVars : [
		'store'
	],
	#category : #FileSystem-Core-Public
}

{ #category : #initializing }
FileSystem class >> startUp: aBoolean [
	"This is only here to deal with migration from older versions of
	FileSystem that wanted to receive startup notifcations."
	
	SessionManager default unregisterClassNamed: self name
]

{ #category : #'instance creation' }
FileSystem class >> store: aStore [
	^ self basicNew
		initializeWithStore: aStore;
		yourself
]

{ #category : #navigating }
FileSystem >> * anObject [
	"Return a relative reference."
	
	^ self referenceTo:( Path * anObject)
]

{ #category : #navigating }
FileSystem >> / anObject [
	"Return the absolute reference obtained by resolving anObject against the
	root of this filesystem."
	
	^ self root / anObject
]

{ #category : #comparing }
FileSystem >> = other [
	^ self species = other species and: [self store = other store]
]

{ #category : #public }
FileSystem >> binaryReadStreamOn: aResolvable [
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a read stream for reading the file."

	^ (self open: aResolvable writable: false) binaryReadStream.
]

{ #category : #public }
FileSystem >> checkName: aString fixErrors: fixErrors [
	^ store checkName: aString fixErrors: fixErrors
]

{ #category : #public-enumerating }
FileSystem >> childNamesAt: aResolvable [
	^ Array streamContents: [ :out | 
		self childNamesAt: aResolvable do: [ :path|
			out nextPut: path ]].
]

{ #category : #public-enumerating }
FileSystem >> childNamesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		nodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]
]

{ #category : #public-enumerating }
FileSystem >> childrenAt: aResolvable [
	^ Array streamContents: [ :out | 
		self childrenAt: aResolvable do: [ :path|
			out nextPut: path ]].
]

{ #category : #public-enumerating }
FileSystem >> childrenAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		nodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]
]

{ #category : #public }
FileSystem >> close [
	store close
]

{ #category : #public }
FileSystem >> copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock [
	"Copy the file referenced as sourcePath to the destination referred as destPath. Perform associate actions in case of problems."

	store
		copy: (self resolve: sourcePath)
		ifAbsent: absentBlock
		to: (self resolve: destinationPath)
		ifPresent: presentBlock
		fileSystem: self
]

{ #category : #public }
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

{ #category : #public }
FileSystem >> copy: aPath toReference: destinationReference [

	^self = destinationReference fileSystem
		ifTrue: [ self copy: aPath to: destinationReference resolve path ]
		ifFalse: [ self copy: aPath toRemote: destinationReference ]
]

{ #category : #public }
FileSystem >> copy: aPath toRemote: destRef [
	| inputStream path |
	path := self resolve: aPath.
	[ inputStream := self readStreamOn: path.
	inputStream ifNil: [ store signalFileDoesNotExist: path ].
	destRef fileSystem copyFrom: inputStream to: destRef path ]
		ensure: [ inputStream ifNotNil: [ inputStream close ] ]
]

{ #category : #public }
FileSystem >> copyAndDelete: sourcePath to: destination [
	"Copy the file referenced as sourcePath to the destination referred as destPath.  
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If an error occurs during the operation, try and roll back to the original state."
	
	[
		self copy: sourcePath toReference: destination.
		self delete: sourcePath.
	] on: Error do: [ :error |
		destination delete.
		error signal.
	]
]

{ #category : #private }
FileSystem >> copyFrom: inputStream to: destPath [
	| buffer out |
	out := nil.
	(self exists: destPath)
		ifTrue: [ store signalFileExists: destPath ].
	^ [ 
	out := self writeStreamOn: destPath.
	buffer := ByteArray new: 1024.
	[ inputStream atEnd ]
		whileFalse: [ 
			buffer := inputStream nextInto: buffer.
			out nextPutAll: buffer ]] ensure: [ out ifNotNil: [ out close ]]
]

{ #category : #public }
FileSystem >> createDirectory: aResolvable [
	"Resolve aResolvable into an absolute path, then as the store to create a directory there. 
	The store is expected to raise an exception if it cannot do so."

	^ store createDirectory: (self resolve: aResolvable)
]

{ #category : #public }
FileSystem >> creationTimeOf: aResolvable [
	"Returns the creation date of aResolvable"

	^ store creationTimeOf: (self resolve: aResolvable)
]

{ #category : #public }
FileSystem >> delete: aResolvable [
	store delete: (self resolve: aResolvable)
]

{ #category : #public }
FileSystem >> delimiter [
	"Return path delimiter used by this filesystem."

	^ store delimiter
]

{ #category : #public-enumerating }
FileSystem >> directoriesAt: aResolvable [
	^ Array streamContents: [ :out | 
		self directoriesAt: aResolvable do: [ :path|
			out nextPut: path ]].
]

{ #category : #public-enumerating }
FileSystem >> directoriesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		directoryNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]
]

{ #category : #public-enumerating }
FileSystem >> directoryNamesAt: aResolvable [
	^ Array streamContents: [ :out | 
		self directoryNamesAt: aResolvable do: [ :name|
			out nextPut: name ]].
]

{ #category : #public-enumerating }
FileSystem >> directoryNamesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		directoryNodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]
]

{ #category : #public }
FileSystem >> ensureCreateDirectory: aResolvable [
	"Resolve the argument to an absolute path, then ask the store to make
	sure that all the directories contained in the argument path exist or are created."
	
	store ensureCreateDirectory: (self resolve: aResolvable)
]

{ #category : #public-enumerating }
FileSystem >> entriesAt: aResolvable [
	^ Array
		streamContents: [ :out | 
			self entriesAt: aResolvable do: [ :entry | out nextPut: entry ]
		]
]

{ #category : #public-enumerating }
FileSystem >> entriesAt: aResolvable do: aBlock [
	^ self 
		entriesAt: aResolvable 
		ifAbsent: [ store signalDirectoryDoesNotExist: ( self resolve: aResolvable) ]
		do: aBlock
]

{ #category : #public-enumerating }
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

{ #category : #public }
FileSystem >> entryAt: aResolvable [
	| path |
	path := self resolve: aResolvable.
	^ store
		nodeAt: path
		ifPresent: [ :node | store entryFromNode: node fileSystem: self path: path ]
		ifAbsent: [ store signalFileDoesNotExist: path ]
]

{ #category : #public }
FileSystem >> exists: aResolvable [
	"Resolve the argument, and answer true if the there is
	a file or directory at that path, false if there is not."
	
	^ store exists: (self resolve: aResolvable)
]

{ #category : #public }
FileSystem >> extensionDelimiter [
	^ $.
]

{ #category : #public-enumerating }
FileSystem >> fileNamesAt: aResolvable [
	^ Array streamContents: [ :out | 
		self fileNamesAt: aResolvable do: [ :path|
			out nextPut: path ]].
]

{ #category : #public-enumerating }
FileSystem >> fileNamesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		fileNodesDo: [ :entry | 
			aBlock value: (store basenameFromEntry: entry) ]
]

{ #category : #public-enumerating }
FileSystem >> filesAt: aResolvable [
	^ Array streamContents: [ :out | 
		self filesAt: aResolvable do: [ :path|
			out nextPut: path ]].
]

{ #category : #public-enumerating }
FileSystem >> filesAt: aResolvable do: aBlock [
	| path |
	path := self resolve: aResolvable.
	store
		directoryAt: path
		ifAbsent: [ store signalDirectoryDoesNotExist: path ]
		fileNodesDo: [ :entry | 
			aBlock value: path / (store basenameFromEntry: entry) ]
]

{ #category : #printing }
FileSystem >> forReferencePrintOn: aStream [
	store forReferencePrintOn: aStream
]

{ #category : #public-testing }
FileSystem >> hasChildren: aResolvable [
	"Returns whether aResolvable has children."
	store directoryAt: (self resolve: aResolvable)
			ifAbsent: [ ^false ]
			nodesDo:  [ :node | ^true ].
	^false
]

{ #category : #public-testing }
FileSystem >> hasDirectories: aResolvable [
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry | 
			entry isDirectory ifTrue: [ ^true ] ].
	^false
]

{ #category : #public-testing }
FileSystem >> hasFiles: aResolvable [
	self
		entriesAt: aResolvable
		ifAbsent: [  ^ false ]
		do: [ :entry | entry isFile ifTrue: [ ^true ] ].
	^false
]

{ #category : #comparing }
FileSystem >> hash [
	^ store hash
]

{ #category : #initialize-release }
FileSystem >> initializeWithStore: aStore [
	store := aStore
]

{ #category : #public-testing }
FileSystem >> isCaseSensitive [
	^ self store isCaseSensitive
]

{ #category : #public-testing }
FileSystem >> isDirectory: aResolvable [
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store isDirectory: (self resolve: aResolvable)
]

{ #category : #public-testing }
FileSystem >> isFile: aResolvable [
	"Resolve the argument, and answer true if the result refers
	to a file, false if it refers to a directory or doesn't exist."

	^ store isFile: (self resolve: aResolvable)
]

{ #category : #public-testing }
FileSystem >> isReadable: aResolvable [
	"Resolve the argument, and answer true if the there is
	a file or directory that can be read from."
	
	^ (self permissions: aResolvable) isReadable
]

{ #category : #public-testing }
FileSystem >> isSymlink: aResolvable [
	"Resolve the argument, and answer true if the result refers
	to a directory, false if it refers to a file or doesn't exist."

	^ store isSymlink: (self resolve: aResolvable)
]

{ #category : #public-testing }
FileSystem >> isWritable: aResolvable [
	"Resolve the argument, and answer true if the there is
	a file that can be written to or directory that can be changed."
	
	^  (self permissions: aResolvable) isWritable
]

{ #category : #public }
FileSystem >> mimeTypesAt: aResolvable [
	"Return the possible mime types for the given path."
	^ store mimeTypesAt: (self resolve: aResolvable)
]

{ #category : #public }
FileSystem >> modificationTime: aResolvable [
	"Returns the date of the last modification of aResolvable"

	^ store modificationTimeOf: (self resolve: aResolvable)
]

{ #category : #public }
FileSystem >> modificationTimeOf: aResolvable [
	"Returns the last date of modification of aResolvable"

	^ store modificationTimeOf: (self resolve: aResolvable)
]

{ #category : #public }
FileSystem >> move: sourcePath to: destination [
	"Move the file /directory referenced as sourcePath to the destination referred as destPath.  
	If there is no file at sourcePath, raise FileDoesNotExist.
	If destPath is a file, raise FileExists.
	If destPath is a directory, move the sourcePath in to the directory"

	| fullDestination |

	destination isFile ifTrue: [ FileExists signalWith: destination ].
	destination isDirectory
		ifTrue: [ fullDestination := destination / sourcePath basename ]
		ifFalse: [ fullDestination := destination ].
	self = destination fileSystem ifTrue: 
	[
		"Ideally we would test whether the source and destination are on the same filesystem from the OSs perspective.
		Since we can't do that, just try rename, and if that fails, copy and delete."
		[ self rename: sourcePath to: fullDestination resolve path ]
			on: Error
			do: [ :error | self copyAndDelete: sourcePath to: fullDestination ].
	] ifFalse:
		[ self copyAndDelete: sourcePath to: fullDestination ].
	^fullDestination
]

{ #category : #public }
FileSystem >> open [
	"Some kinds of filesystems need to open connections to external resources. Does nothing by default."
	
	store open
]

{ #category : #public }
FileSystem >> open: aResolvable writable: aBoolean [ 
	"Resolve aResolvable into an absolute path, then ask the store to open the file at
	that path using the specified access mode."
	
	| path |
	path := self resolve: aResolvable.
	^ store handleClass 
		open: (FileReference fileSystem: self path: path) 
		writable: aBoolean
]

{ #category : #delegated }
FileSystem >> openFileStream: aResolvable writable: aBoolean [
	^ store 
		openFileStream: (self resolve: aResolvable) 
		writable: aBoolean
]

{ #category : #private }
FileSystem >> openStreamDescription: aResolvable writable: aBoolean [
	"I am  a helper method to delegate basicOpen:writable: to the store.
	 I am called from FileSystemHandle implementations."
	
	| path |
	
	path := self resolve: aResolvable.
	^ store basicOpen: path writable: aBoolean
]

{ #category : #converting }
FileSystem >> pathFromObject: anObject [ 
	^ anObject asPathWith: self
]

{ #category : #converting }
FileSystem >> pathFromString: aString [
	^ store pathFromString: aString
]

{ #category : #public }
FileSystem >> permissions: aResolvable [
	"Resolve the argument and return the Permissions for this file or directory "

	^ store permissions: (self resolve: aResolvable)
]

{ #category : #printing }
FileSystem >> printPath: aPath on: aStream [
	store printPath: aPath on: aStream
]

{ #category : #public }
FileSystem >> readStreamOn: aResolvable [
	"Resolve the argument into an absolute path and open a file handle on the file
	at that path. Ask the handle to give us a read stream for reading the file."

	^ (self open: aResolvable writable: false) readStream.
]

{ #category : #public }
FileSystem >> referenceTo: aResolvable [ 
	"Answer a reference to the argument from the context of the receiver filesystem.  	
		Example: Filesystem disk referenceTo: 'plonk.taz'"

	^ FileReference 
		fileSystem: self
		path: (self pathFromObject: aResolvable)
]

{ #category : #public }
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

{ #category : #public }
FileSystem >> rename: sourcePath to: destName [
	"Rename the file referenced as sourcePath to destPath.  Raise exceptions 
	FileExists or FileDoesNotExist  if the operation fails"
	
	^ self
		rename: sourcePath
		ifAbsent: [store signalFileDoesNotExist: sourcePath]
		to: destName
		ifPresent: [store signalFileExists: destName]
]

{ #category : #public }
FileSystem >> resolve: aResolvable [
	^ aResolvable asResolvedBy: self
]

{ #category : #navigating }
FileSystem >> resolvePath: aPath [
	"Return a path where the argument is resolved in the context of the
	receiver. The behavior is similar to the one of a command line.
		> cd /a/b/c
		> cd b
		The shell will attempt to make /a/b/c/b the current directory. "

	^ self workingDirectoryPath resolve: aPath
]

{ #category : #navigating }
FileSystem >> resolveReference: aReference [
	^ aReference fileSystem = self ifTrue:
		[self workingDirectoryPath resolvePath: aReference path]
]

{ #category : #navigating }
FileSystem >> resolveString: aString [
	"Returns the root of the receiver filesystem, i.e. / on unix"
	
	^ self workingDirectoryPath resolvePath: (store pathFromString: aString)
]

{ #category : #accessing }
FileSystem >> root [
	"Returns the root of the receiver filesystem, i.e. / on unix"
	^ self referenceTo: Path root
]

{ #category : #accessing }
FileSystem >> separator [
	"Return path separator used by this filesystem."

	^ store separator
]

{ #category : #public }
FileSystem >> sizeOf: aResolvable [
	"Resolve the argument and return the size for this file or directory "

	^ store sizeOf: (self resolve: aResolvable)
]

{ #category : #accessing }
FileSystem >> store [
	^ store
]

{ #category : #converting }
FileSystem >> stringFromPath: aPath [
	^ store stringFromPath: aPath
]

{ #category : #accessing }
FileSystem >> workingDirectory [
	"Returns a reference to the directory from where the image was launched"

	^ self referenceTo: self workingDirectoryPath
]

{ #category : #accessing }
FileSystem >> workingDirectoryPath [
	^ store defaultWorkingDirectory
]

{ #category : #public }
FileSystem >> writeStreamOn: aResolvable [
	"Open a write stream on the file referred by the argument. It can be a string or a path"

	^ (self open: aResolvable writable: true) writeStream.
]
