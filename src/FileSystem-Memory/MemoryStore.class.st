"
I'm a specific store for memory file system
"
Class {
	#name : #MemoryStore,
	#superclass : #FileSystemStore,
	#instVars : [
		'root'
	],
	#category : #'FileSystem-Memory-Store'
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

{ #category : #private }
MemoryStore >> basicCreationTimeOf: aMemoryFileSystemEntry [
	"Returns the creation date of aMemoryFileSystemEntry"
	^ aMemoryFileSystemEntry creationTime
]

{ #category : #private }
MemoryStore >> basicEntry: entry nodesDo: aBlock [
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
	^ aMemoryFileSystemEntry isFile
]

{ #category : #private }
MemoryStore >> basicIsSymlink: aNode [
	^false
]

{ #category : #private }
MemoryStore >> basicModificationTimeOf: aMemoryFileSystemEntry [
	"Return the basic modification time of aMemoryFileSystemEntry"
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
	^ 8r777
]

{ #category : #private }
MemoryStore >> basicSizeOf: aMemoryFileSystemEntry [
	"Return the basic size of aMemoryFileSystemEntry"
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

{ #category : #public }
MemoryStore >> delete: path [
	self 
		nodeAt: path parent
		ifPresent: [ :dict | 
			dict fileEntryRemove: path basename ifAbsent: [ FileDoesNotExist signalWith: path ]] 
		ifAbsent: [ DirectoryDoesNotExist signalWith: path parent ]
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
MemoryStore >> isMemoryFileSystem [
	^ true
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
