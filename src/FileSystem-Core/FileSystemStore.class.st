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
	self shouldBeImplemented 
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

{ #category : #public }
FileSystemStore >> basenameFromNode: aNode [
	"Used to extract the basename from the low-level representation (node / entry) from the store."
	self subclassResponsibility
]

{ #category : #deprecated }
FileSystemStore >> basicCreationTimeOf: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility 
]

{ #category : #abstract }
FileSystemStore >> basicEntry: directoryEntry path: aPath nodesDo: aBlock [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility 
]

{ #category : #abstract }
FileSystemStore >> basicIsDirectory: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is a directory.
	This private message should only be called form within the store."
	self subclassResponsibility 
]

{ #category : #deprecated }
FileSystemStore >> basicIsFile: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is a file.
	This private message should only be called form within the store."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility 
]

{ #category : #deprecated }
FileSystemStore >> basicIsSymlink: aNode [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^self subclassResponsibility 
	
]

{ #category : #deprecated }
FileSystemStore >> basicIsWritable: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is 
	a writable file or can be changed."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility 
]

{ #category : #deprecated }
FileSystemStore >> basicModificationTime: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is a readable
	file or a directory whose contents can be listed."
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility 
]

{ #category : #'error signalling' }
FileSystemStore >> basicOpen: aPath writable: aBoolean [
	"open the file at the given path and return an identifier"
	self subclassResponsibility
]

{ #category : #deprecated }
FileSystemStore >> basicPosixPermissions: aNode [
	"Used to get the posix permissions from a low-level filesystem entry / node"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility 
]

{ #category : #deprecated }
FileSystemStore >> basicSizeOf: aNode [
	"Used to get the size of the low-level representation (node / entry) "
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	self subclassResponsibility 
]

{ #category : #testing }
FileSystemStore >> checkExists: aPath [

	(self exists: aPath) ifFalse:
		[ FileDoesNotExist signalWith: aPath ].
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
		in := aFilesystem readStreamOn: sourcePath.
		in ifNil: [ ^ absentBlock value ].
		
		(self exists: destinationPath)
			ifTrue: [ "cannot overwrite destination"
				^ presentBlock value ].
			
		out := aFilesystem writeStreamOn: destinationPath.
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

{ #category : #accessing }
FileSystemStore >> creationTimeOf: aPath [
	"Return the date of creation of the File described by aPath.
	Unlike most attribute methods in the file system store, retrieve the attributes object for aPath to minimise the number of primitive calls as creationTime often needs to get change time as well."
	^FileAttributeNotSupported signalWith: self
	
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
		nodesAt: aPath 
		do: [ :node |
			(self basicIsDirectory: node) 
				ifTrue: [ aBlock value: node ]].
]

{ #category : #public }
FileSystemStore >> directoryAt: aPath fileNodesDo: aBlock [
	^ self 
		nodesAt: aPath 
		do: [ :node |
			(self basicIsDirectory: node) 
				ifFalse: [ aBlock value: node ]].
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

	^[ self nodesAt: aPath do: aBlock ]
		on: DirectoryDoesNotExist, FileDoesNotExist
		do: [ absentBlock value ].

]

{ #category : #public }
FileSystemStore >> ensureCreateDirectory: aPath [
	(self isDirectory: aPath) ifTrue: [ ^ self ].
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

{ #category : #private }
FileSystemStore >> entryFromNode: aNode fileSystem: aFilesystem path: aPath [

	self 
		deprecated: 'FileSystemDirectoryEntry class>>fileSystem:path:'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
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
	^self subclassResponsibility
	
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

{ #category : #testing }
FileSystemStore >> isDirectory: aPath [
	^FileAttributeNotSupported signalWith: self
	
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

{ #category : #testing }
FileSystemStore >> isFile: aPath [
	"Return a boolean indicating whether the File described by aPath is a file (not a directory)"
	^FileAttributeNotSupported signalWith: self
	
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

{ #category : #testing }
FileSystemStore >> isSymlink: aPath [
	^FileAttributeNotSupported signalWith: self
]

{ #category : #testing }
FileSystemStore >> isWritable: aPath [
	^FileAttributeNotSupported signalWith: self
]

{ #category : #public }
FileSystemStore >> mimeTypesAt: aPath [
	| mimeType |
	mimeType := ZnMimeType 
						forFilenameExtension: aPath extension 
						ifAbsent: [  ^ nil ].
	^ Array with: mimeType
]

{ #category : #accessing }
FileSystemStore >> modificationTimeOf: aPath [
	"Returns the last date of modification of the File described by aPath"
	^FileAttributeNotSupported signalWith: self
]

{ #category : #public }
FileSystemStore >> nodeAt: aPath [
	self 
		deprecated: 'Use FileSystemDirectoryEntry instead'
		on: 	'19 Feb 2018' 
		in: 'Pharo-7.0'.
	^ self 
		nodeAt: aPath 
		ifPresent: [ :node| node ]
		ifAbsent: [ NotFound signalFor: aPath in: self ]
]

{ #category : #abstract }
FileSystemStore >> nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^self subclassResponsibility 
]

{ #category : #private }
FileSystemStore >> nodesAt: aPath do: aBlock [

	^self subclassResponsibility
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

{ #category : #public }
FileSystemStore >> openFileStream: path writable: writable [
	self subclassResponsibility
]

{ #category : #converting }
FileSystemStore >> pathFromString: aString [
	"Use the unix convention by default, since many filesystems are based on it."
	
	^ Path from: aString delimiter: self delimiter
]

{ #category : #accessing }
FileSystemStore >> permissions: aPath [
	^FileAttributeNotSupported signalWith: self
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
	^ FileDoesNotExist signalWith: aPath
]

{ #category : #'error signalling' }
FileSystemStore >> signalFileExists: aPath [
	^ FileExists signalWith: aPath
]

{ #category : #accessing }
FileSystemStore >> sizeOf: aPath [
	"Return the size of the File described by aPath"
	^FileAttributeNotSupported signalWith: self
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
