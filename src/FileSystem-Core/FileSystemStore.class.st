"
I am an abstract superclass for store implementations. My subclasses provide access to the actual data storage of a particular kind of filesystem. 

The file system can be accessed via
	FileSystem disk 
	FileSystem memory
	
My associated filesystem can be accessed as follows:
      DiskStore currentFileSystem
"
Class {
	#name : #FileSystemStore,
	#superclass : #Object,
	#category : #'FileSystem-Core-Kernel'
}

{ #category : #accessing }
FileSystemStore class >> delimiter [
	^ self shouldBeImplemented 
]

{ #category : #accessing }
FileSystemStore class >> isCaseSensitive [
	^ self shouldBeImplemented
]

{ #category : #accessing }
FileSystemStore class >> separator [
	self shouldBeImplemented
]

{ #category : #accessing }
FileSystemStore >> accessTimeOf: aPath [
	"Return the date of last access of the File described by aPath"
	^FileAttributeNotSupported signalWith: self
	
]

{ #category : #public }
FileSystemStore >> basenameFromEntry: aNode [
	"Used to extract the basename from the low-level representation (node / entry) from the store."
	self subclassResponsibility
]

{ #category : #abstract }
FileSystemStore >> basicCreationTimeOf: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self subclassResponsibility 
]

{ #category : #abstract }
FileSystemStore >> basicEntry: directoryEntry path: aPath nodesDo: aBlock [
	self subclassResponsibility 
]

{ #category : #abstract }
FileSystemStore >> basicIsDirectory: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is a directory.
	This private message should only be called form within the store."
	self subclassResponsibility 
]

{ #category : #abstract }
FileSystemStore >> basicIsFile: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is a file.
	This private message should only be called form within the store."
	self subclassResponsibility 
]

{ #category : #abstract }
FileSystemStore >> basicIsSymlink: aNode [
	^self subclassResponsibility 
	
]

{ #category : #abstract }
FileSystemStore >> basicIsWritable: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is 
	a writable file or can be changed."
	self subclassResponsibility 
]

{ #category : #abstract }
FileSystemStore >> basicModificationTime: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self subclassResponsibility 
]

{ #category : #'error signalling' }
FileSystemStore >> basicOpen: aPath writable: aBoolean [
	"open the file at the given path and return an identifier"
	self subclassResponsibility
]

{ #category : #abstract }
FileSystemStore >> basicPosixPermissions: aNode [
	"Used to get the posix permissions from a low-level filesystem entry / node"
	self subclassResponsibility 
]

{ #category : #abstract }
FileSystemStore >> basicSizeOf: aNode [
	"Used to get the size of the low-level representation (node / entry) "
	self subclassResponsibility 
]

{ #category : #testing }
FileSystemStore >> checkExists: aPath [

	(self exists: aPath) ifFalse:
		[ FileDoesNotExistException signalWith: aPath ].
]

{ #category : #public }
FileSystemStore >> checkName: aString fixErrors: fixErrors [
	^ self subclassResponsibility
]

{ #category : #abstract }
FileSystemStore >> close [
	"Some kinds of filesystems need to open connections to external resources"
]

{ #category : #private }
FileSystemStore >> copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: aFilesystem [

	| buffer out in |
	
	in := nil.
	out := nil.
	buffer := nil.
	[
		in := aFilesystem binaryReadStreamOn: sourcePath.
		in ifNil: [ ^ absentBlock value ].
		
		(self exists: destinationPath)
			ifTrue: [ "cannot overwrite destination"
				^ presentBlock value ].
			
		out := aFilesystem binaryWriteStreamOn: destinationPath.
		buffer := ByteArray new: 1024.
		
		[ in atEnd ]
			whileFalse: [ 
				buffer := in nextInto: buffer.
				out nextPutAll: buffer ]]
	ensure: [ 
		in ifNotNil: [ in close ].
		out ifNotNil: [ out close ]]
]

{ #category : #abstract }
FileSystemStore >> createDirectory: aPath [
	self subclassResponsibility 
]

{ #category : #public }
FileSystemStore >> creationTimeOf: aPath [
	"Return the date of creation of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicCreationTimeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ].
	
]

{ #category : #accessing }
FileSystemStore >> defaultWorkingDirectory [
	^ Path root
]

{ #category : #abstract }
FileSystemStore >> delete: aPath [
	self subclassResponsibility 
]

{ #category : #accessing }
FileSystemStore >> delimiter [
	^ self class delimiter
]

{ #category : #accessing }
FileSystemStore >> deviceIdOf: aPath [
	"Return the device id of the File described by aPath"
	^FileAttributeNotSupported signalWith: self
]

{ #category : #public }
FileSystemStore >> directoryAt: aPath  directoryNodesDo: aBlock [
	^ self 
		directoryAt: aPath 
		nodesDo: [ :entry|
			(self basicIsDirectory: entry) 
				ifTrue: [ aBlock value: entry ]].
]

{ #category : #public }
FileSystemStore >> directoryAt: aPath fileNodesDo: aBlock [
	^ self 
		directoryAt: aPath 
		nodesDo: [ :entry|
			(self basicIsDirectory: entry) 
				ifFalse: [ aBlock value: entry ]].
]

{ #category : #public }
FileSystemStore >> directoryAt: aPath ifAbsent: absentBlock directoryNodesDo: aBlock [
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :entry|
			(self basicIsDirectory: entry) 
				ifTrue: [ aBlock value: entry ]].
]

{ #category : #public }
FileSystemStore >> directoryAt: aPath ifAbsent: absentBlock fileNodesDo: aBlock [
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :entry|
			(self basicIsDirectory: entry) 
				ifFalse: [ aBlock value: entry ]].
]

{ #category : #public }
FileSystemStore >> directoryAt: aPath ifAbsent: absentBlock nodesDo: aBlock [
	^ self
		nodeAt: aPath
		ifPresent: [ :entry | 
			(self basicIsDirectory: entry)
				ifTrue: [ self basicEntry: entry path: aPath nodesDo: aBlock ]
				ifFalse: [ DirectoryDoesNotExist signalWith: aPath ] ]
		ifAbsent: absentBlock
]

{ #category : #public }
FileSystemStore >> directoryAt: aPath nodesDo: aBlock [
	^ self
		nodeAt: aPath
		ifPresent: [ :entry | 
			(self basicIsDirectory: entry)
				ifTrue: [ self basicEntry: entry path: aPath nodesDo: aBlock ]
				ifFalse: [ DirectoryDoesNotExist signalWith: aPath ] ]
		ifAbsent: [ self signalDirectoryDoesNotExist: aPath ]
]

{ #category : #public }
FileSystemStore >> ensureCreateDirectory: aPath [
	((self exists: aPath) and: [ self isDirectory: aPath]) ifTrue: [ ^ self ].
	self ensureCreateDirectory: aPath parent.
	self createDirectory: aPath
]

{ #category : #private }
FileSystemStore >> entryAt: aPath fileSystem: aFilesystem [

	self checkExists: aPath.
	^ FileSystemDirectoryEntry
		fileSystem: aFilesystem
		path: aPath
]

{ #category : #public }
FileSystemStore >> entryFromNode: node path: path for: aFileystem [
	^self subclassResponsibility
]

{ #category : #public }
FileSystemStore >> exists: aPath [
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ true ] 
		ifAbsent: [ ^ false ].
	
]

{ #category : #private }
FileSystemStore >> filename: aByteString matches: aByteString2 [
	^ aByteString = aByteString2
]

{ #category : #accessing }
FileSystemStore >> gidOf: aPath [
	"Return the gid of the File described by aPath"
	^FileAttributeNotSupported signalWith: self

]

{ #category : #abstract }
FileSystemStore >> inodeOf: aPath [
	"Return the inode number of the File described by aPath"
	^FileAttributeNotSupported signalWith: self

]

{ #category : #testing }
FileSystemStore >> isBlock: aPath [
	"Return a boolean indicating whether the File described by aPath is a block device"
	^FileAttributeNotSupported signalWith: self
	
]

{ #category : #accessing }
FileSystemStore >> isCaseSensitive [
	^ self class isCaseSensitive
]

{ #category : #testing }
FileSystemStore >> isCharacter: aPath [
	"Return a boolean indicating whether the File described by aPath is character based"
	^FileAttributeNotSupported signalWith: self
	
]

{ #category : #public }
FileSystemStore >> isDirectory: aPath [
	aPath isRoot ifTrue: [ ^ true ].
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsDirectory: entry ] 
		ifAbsent: [ ^ false ].
	
]

{ #category : #testing }
FileSystemStore >> isExecutable: aPath [
	^FileAttributeNotSupported signalWith: self
]

{ #category : #testing }
FileSystemStore >> isFIFO: aPath [
	"Return a boolean indicating whether the File described by aPath is FIFO"
	^FileAttributeNotSupported signalWith: self
	
]

{ #category : #public }
FileSystemStore >> isFile: aPath [
	"slow solution for big directories! "
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsFile: entry ] 
		ifAbsent: [ ^ false ]
	
]

{ #category : #testing }
FileSystemStore >> isReadable: aPath [
	^FileAttributeNotSupported signalWith: self
]

{ #category : #testing }
FileSystemStore >> isRegular: aPath [
	"Return a boolean indicating whether the File described by aPath is a regular file"
	^FileAttributeNotSupported signalWith: self
	
]

{ #category : #testing }
FileSystemStore >> isSocket: aPath [
	"Return a boolean indicating whether the File described by aPath is a socket"
	^FileAttributeNotSupported signalWith: self
]

{ #category : #public }
FileSystemStore >> isSymlink: aPath [
	aPath isRoot ifTrue: [ ^ true ].
	self 
		nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsSymlink: entry ] 
		ifAbsent: [ ^ false ].
	
]

{ #category : #public }
FileSystemStore >> isWritable: aPath [
	self nodeAt: aPath 
		ifPresent: [ :entry | ^ self basicIsWritable: entry ] 
		ifAbsent: [ ^ false ].
	
]

{ #category : #public }
FileSystemStore >> modificationTimeOf: aPath [
	"Returns the last date of modification of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicModificationTimeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ].
	
]

{ #category : #public }
FileSystemStore >> nodeAt: aPath [
	^ self 
		nodeAt: aPath 
		ifPresent: [ :node| node ]
		ifAbsent: [ NotFound signalFor: aPath in: self ]
]

{ #category : #abstract }
FileSystemStore >> nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock [
	self subclassResponsibility 
]

{ #category : #accessing }
FileSystemStore >> numberOfHardLinks: aPath [
	"Return the number of hard links to the File described by aPath"
	^FileAttributeNotSupported signalWith: self
]

{ #category : #abstract }
FileSystemStore >> open [
	"Some kinds of filesystems need to open connections to external resources"
]

{ #category : #converting }
FileSystemStore >> pathFromString: aString [
	"Use the unix convention by default, since many filesystems are based on it."
	
	^ Path from: aString delimiter: self delimiter
]

{ #category : #public }
FileSystemStore >> permissions: aPath [
	self nodeAt: aPath 
		ifPresent: [ :entry | ^ FileSystemPermission posixPermissions: (self basicPosixPermissions: entry) ] 
		ifAbsent: [ ^ FileSystemPermission default ].
	
]

{ #category : #converting }
FileSystemStore >> printPath: aPath on: out [
	"Use the unix convention by default, since it's the most common."
	
	aPath isAbsolute ifTrue: [ out nextPut: self delimiter ].
	^ aPath printOn: out delimiter: self delimiter
]

{ #category : #private }
FileSystemStore >> rename: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: anFSFilesystem [

	| result |
	(self exists: destinationPath) ifTrue: [ ^ presentBlock value ].
	(self exists: sourcePath) ifFalse: [ ^ absentBlock value ].
	result := self rename: sourcePath to: destinationPath.
	result ifNil: [ self primitiveFailed ].
	^ self.
]

{ #category : #abstract }
FileSystemStore >> rename: sourcePath to: destinationPath [
	self subclassResponsibility 
]

{ #category : #accessing }
FileSystemStore >> separator [
	^ self class separator
]

{ #category : #'error signalling' }
FileSystemStore >> signalDirectoryDoesNotExist: aPath [
	^ DirectoryDoesNotExist signalWith: aPath
]

{ #category : #'error signalling' }
FileSystemStore >> signalDirectoryExists: aPath [
	^ DirectoryExists signalWith: aPath
]

{ #category : #'error signalling' }
FileSystemStore >> signalFileDoesNotExist: aPath [
	^ FileDoesNotExistException
		signalWithFile: (File named: aPath asPath pathString)
		writeMode: false
]

{ #category : #'error signalling' }
FileSystemStore >> signalFileExists: aPath [
	^ FileExists signalWith: aPath
]

{ #category : #public }
FileSystemStore >> sizeOf: aPath [
	"Return the size of the File described by aPath"
	^ self 
		nodeAt: aPath 
		ifPresent: [ :entry | self basicSizeOf: entry ] 
		ifAbsent: [ self signalFileDoesNotExist: aPath ]
	
]

{ #category : #converting }
FileSystemStore >> stringFromPath: aPath [
	^ String streamContents: [ :out | 
		self printPath: aPath on: out ]
]

{ #category : #private }
FileSystemStore >> symlinkEntryAt: aPath fileSystem: aFilesystem [

	^ DiskSymlinkDirectoryEntry
		fileSystem: aFilesystem
		path: aPath
]

{ #category : #accessing }
FileSystemStore >> targetPath: aPath [
	"Return the target file of the File described by aPath.  For a regular file, this is itself, for a symbolic link, it is the file pointed to by the symbolic link"
	^FileAttributeNotSupported signalWith: self
]

{ #category : #accessing }
FileSystemStore >> uid: aPath [
	"Return the uid of the File described by aPath"
	^FileAttributeNotSupported signalWith: self
]
