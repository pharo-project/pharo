"
I'm a specific store for memory file system
"
Class {
	#name : #MemoryStore,
	#superclass : #FileSystemStore,
	#instVars : [
		'root'
	],
	#classVars : [
		'CurrentFS'
	],
	#category : #'FileSystem-Memory-Store'
}

{ #category : #current }
MemoryStore class >> currentFileSystem [
	^ CurrentFS ifNil: [ CurrentFS := FileSystem store: MemoryStore new ]
]

{ #category : #public }
MemoryStore class >> delimiter [
	^ $/
]

{ #category : #'class initialization' }
MemoryStore class >> initialize [
	"self initialize"

	SessionManager default registerSystemClassNamed: self name
]

{ #category : #public }
MemoryStore class >> isCaseSensitive [
	^ true
]

{ #category : #initialization }
MemoryStore class >> reset [
	CurrentFS := nil
]

{ #category : #public }
MemoryStore class >> separator [ 
	^ $:
]

{ #category : #'system startup' }
MemoryStore class >> startUp [
	self reset
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
		ifPresent: [ :aMemoryFileSystemEntry | 
			aMemoryFileSystemEntry
				basicOpen;
				yourself ]
		ifAbsent: [ aBoolean
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
			dict fileEntryRemove: path basename ifAbsent: [ FileDoesNotExistException signalWith: path ]] 
		ifAbsent: [ DirectoryDoesNotExist signalWith: path parent ]
]

{ #category : #public }
MemoryStore >> entryFromNode: node path: path for: aFileSystem [

	| entryPath |
	entryPath := path / (self basenameFromEntry: node).
	^MemoryDirectoryEntry reference: (FileReference fileSystem: aFileSystem path: entryPath).

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

{ #category : #testing }
MemoryStore >> isReadable: aPath [
	"Answer a boolean indicating whether the supplied path is a readable entry (directory or file).
	MemoryStore doesn't implement access controls, always true if the file exists."

	^self exists: aPath

]

{ #category : #accessing }
MemoryStore >> isRegular: aPath [
	"Answer a boolean indicating whether the entry at aPath is writable.
	Memory store only supports regular files."

	^self isFile: aPath
]

{ #category : #testing }
MemoryStore >> isWritable: aPath [
	"Answer a boolean indicating whether the supplied path is a writable entry (directory or file).
	MemoryStore doesn't implement access controls, always true if the file exists."

	^self exists: aPath

]

{ #category : #private }
MemoryStore >> nodeAt: aPath [
	| current |
	current := self root.
	aPath do: [ :segment | 
		current isDirectory
			ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ FileDoesNotExistException signalWith: aPath ]]
			ifFalse: [ ^ FileDoesNotExistException signalWith: aPath ]].
	^ current
]

{ #category : #private }
MemoryStore >> nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock [
	| current |
	aPath isRoot
		ifTrue: [ ^ presentBlock value: self root ].
	current := self root.
	aPath
		do: [ :segment | 
			current isDirectory
				ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ absentBlock value ] ]
				ifFalse: [ ^ absentBlock value ] ].
	^ presentBlock value: current
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
