"
I'm a specific store for memory file system
"
Class {
	#name : #MemoryStore,
	#superclass : #FileSystemStore,
	#instVars : [
		'root'
	],
	#category : #'FileSystem-Memory'
}

{ #category : #public }
MemoryStore class >> delimiter [
	^ $/
]

{ #category : #public }
MemoryStore class >> isCaseSensitive [
	^ true
]

{ #category : #public }
MemoryStore class >> separator [ 
	^ $:
]

{ #category : #public }
MemoryStore >> basenameFromEntry: aMemoryFileSystemEntry [
	^ aMemoryFileSystemEntry basename
]

{ #category : #public }
MemoryStore >> basenameFromNode: aMemoryFileSystemEntry [
	^ aMemoryFileSystemEntry basename
]

{ #category : #private }
MemoryStore >> basicCreationTimeOf: aMemoryFileSystemEntry [
	"Returns the creation date of aMemoryFileSystemEntry"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry creationTime
]

{ #category : #private }
MemoryStore >> basicEntry: entry nodesDo: aBlock [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	entry fileEntriesDo: aBlock
]

{ #category : #private }
MemoryStore >> basicEntry: directoryEntry path: aPath nodesDo: aBlock [
	directoryEntry fileEntriesDo: aBlock
]

{ #category : #private }
MemoryStore >> basicIsDirectory: aMemoryFileSystemEntry [
	^ aMemoryFileSystemEntry isDirectory
]

{ #category : #private }
MemoryStore >> basicIsFile: aMemoryFileSystemEntry [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry isFile
]

{ #category : #private }
MemoryStore >> basicIsSymlink: aNode [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^false
]

{ #category : #private }
MemoryStore >> basicModificationTimeOf: aMemoryFileSystemEntry [
	"Return the basic modification time of aMemoryFileSystemEntry"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry modificationTime
]

{ #category : #private }
MemoryStore >> basicOpen: path writable: aBoolean [
	^ self
		nodeAt: path
		ifPresent: [ :aMemoryFileSystemEntry | aMemoryFileSystemEntry ]
		ifAbsent: [ 
			aBoolean
				ifFalse: [ self signalFileDoesNotExist: path ]
				ifTrue: [ self createFile: path ] ]
]

{ #category : #private }
MemoryStore >> basicPosixPermissions: anEntry [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ 8r777
]

{ #category : #private }
MemoryStore >> basicSizeOf: aMemoryFileSystemEntry [
	"Return the basic size of aMemoryFileSystemEntry"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ aMemoryFileSystemEntry fileSize
]

{ #category : #public }
MemoryStore >> checkName: aString fixErrors: fixErrors [
	aString ifEmpty: [ self error: 'zero length file name' ].
	^ aString
]

{ #category : #private }
MemoryStore >> copy: sourcePath ifAbsent: absentBlock to: destinationPath ifPresent: presentBlock fileSystem: aFilesystem [
        | sourceNode destinationNode |

        sourceNode := self
                nodeAt: sourcePath
                ifPresent: [ :source | source ]
                ifAbsent: [ ^ absentBlock value].

        sourceNode isDirectory
                ifTrue: [ ^ absentBlock value ].

        destinationNode := self
                nodeAt: destinationPath parent
                ifPresent: [ :destination |  destination ]
                ifAbsent: [ ^ self signalDirectoryDoesNotExist: destinationPath parent ].

        destinationNode isFile
                ifTrue: [ self signalDirectoryDoesNotExist: destinationPath parent ].

        (destinationNode fileEntriesIncludes: destinationPath basename)
                ifTrue: [ "cannot overwrite existing file"^ presentBlock value ].

        destinationNode
                fileEntryAt: destinationPath basename
                put: (sourceNode copy
                                        basename: destinationPath basename;
                                        yourself)

]

{ #category : #public }
MemoryStore >> createDirectory: path [
	| parent |
	parent := path parent.
	^ self
		nodeAt: parent
		ifPresent: [ :entry | 
			entry
				fileEntryAt: path basename
				ifPresent: [ :node | 
					node isDirectory
						ifTrue: [ self signalDirectoryExists: path ]
						ifFalse: [ self signalFileExists: path ] ].
			entry ensureCreateDirectory: path basename  ]
		ifAbsent: [ self signalDirectoryDoesNotExist: parent ]
]

{ #category : #private }
MemoryStore >> createFile: aPath [
	^ self
		nodeAt: aPath parent
		ifPresent: [ :entry | 
			entry isDirectory
				ifTrue: [ entry ensureCreateFile: aPath basename ]]
		ifAbsent: [ self signalDirectoryDoesNotExist: aPath parent ]
]

{ #category : #accessing }
MemoryStore >> creationTimeOf: aPath [

	^(self nodeAt: aPath) creationTime
]

{ #category : #public }
MemoryStore >> delete: path [
	self 
		nodeAt: path parent
		ifPresent: [ :dict | 
			dict fileEntryRemove: path basename ifAbsent: [ FileDoesNotExist signalWith: path ]] 
		ifAbsent: [ DirectoryDoesNotExist signalWith: path parent ]
]

{ #category : #public }
MemoryStore >> entryFromNode: node path: path for: aFileSystem [

	| entryPath |
	entryPath := path / (self basenameFromNode: node).
	^MemoryDirectoryEntry reference: (FileReference fileSystem: aFileSystem path: entryPath).

]

{ #category : #testing }
MemoryStore >> exists: aPath [

	^self nodeAt: aPath
		ifPresent: [ :node | true ]
		ifAbsent: [ false ]
]

{ #category : #printing }
MemoryStore >> forReferencePrintOn: aStream [
	aStream nextPutAll: 'memory://'
]

{ #category : #accessing }
MemoryStore >> handleClass [
	^ MemoryHandle 
]

{ #category : #initialization }
MemoryStore >> initialize [ 
	root := MemoryFileSystemDirectory new
]

{ #category : #testing }
MemoryStore >> isDirectory: aPath [

	^self nodeAt: aPath
		ifPresent: [ :node | node isDirectory ]
		ifAbsent: [ false ]
]

{ #category : #testing }
MemoryStore >> isFile: aPath [

	^self nodeAt: aPath
		ifPresent: [ :node | node isFile ]
		ifAbsent: [ false ]
]

{ #category : #testing }
MemoryStore >> isMemoryFileSystem [
	^ true
]

{ #category : #accessing }
MemoryStore >> modificationTimeOf: aPath [

	^(self nodeAt: aPath) modificationTime
]

{ #category : #private }
MemoryStore >> nodeAt: aPath [
	| current |
	current := self root.
	aPath do: [ :segment | 
		current isDirectory
			ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ FileDoesNotExist signalWith: aPath ]]
			ifFalse: [ ^ FileDoesNotExist signalWith: aPath ]].
	^ current
]

{ #category : #private }
MemoryStore >> nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock [
	| current |
	current := self root.
	aPath do: [ :segment | 
		current isDirectory
			ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ absentBlock value ]]
			ifFalse: [ ^ absentBlock value ]].
	^ presentBlock value: current
]

{ #category : #private }
MemoryStore >> nodesAt: aPath do: aBlock [

	^(self nodeAt: aPath) fileEntriesDo:
		[ :each | aBlock value: each ]. 
]

{ #category : #public }
MemoryStore >> openFileStream: path writable: isWriteStream [
	| entry |
	entry := self basicOpen: path writable: isWriteStream.
	^ isWriteStream 
		ifTrue: [ entry writeStream ]
		ifFalse: [ entry readStream ]
]

{ #category : #public }
MemoryStore >> rename: sourcePath to: destinationPath [
	| sourceEntry destinationParentEntry newName |
	
	sourceEntry := self nodeAt: sourcePath.
	newName := destinationPath basename.
	
	destinationParentEntry := self nodeAt: destinationPath parent.
	
	destinationParentEntry isDirectory
		ifFalse: [ Error signal: 'Copy destination has to be a directory' ].
	destinationParentEntry 
		fileEntryAt: newName
		ifPresent: [ Error signal: 'Destination file exists already' ].
		
	destinationParentEntry 
		fileEntryAt: newName
		put: sourceEntry.
	sourceEntry basename: newName.
	
	
	(self nodeAt: sourcePath parent)
		fileEntryRemove: sourcePath basename 
	
]

{ #category : #private }
MemoryStore >> replaceFile: path in: aBlock [
	^ self
		nodeAt: path parent
		ifPresent: [ :entry | | old new |
			entry isDirectory
				ifFalse: [ self signalFileDoesNotExist: path ].
			old := entry fileEntryAt: path basename ifAbsent: [ self signalFileDoesNotExist: path ].
			new := aBlock value: old.
			entry fileEntryAt: path basename put: new ]
		ifAbsent: [ self signalFileDoesNotExist: path ]
]

{ #category : #accessing }
MemoryStore >> root [
	^ root
]

{ #category : #accessing }
MemoryStore >> sizeOf: aPath [

	^(self nodeAt: aPath) fileSize
]
