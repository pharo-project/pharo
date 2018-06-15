"
I am an abstract superclass for disk store implementations. My subclasses provide access to the actual data storage of a particular kind of filesystem. 

"
Class {
	#name : #DiskStore,
	#superclass : #FileSystemStore,
	#instVars : [
		'maxFileNameLength'
	],
	#classVars : [
		'CurrentFS',
		'DefaultWorkingDirectory'
	],
	#category : #'FileSystem-Disk-Store'
}

{ #category : #current }
DiskStore class >> activeClass [
	self allSubclassesDo: [:ea | 
		ea isActiveClass ifTrue: [^ ea]].
	^ self
]

{ #category : #'system startup' }
DiskStore class >> checkVMVersion [
	"Display a warning if the VM is too old"
	| displayError |
	displayError := [ ^ self inform: 'Your VM is too old for this image. Please download the latest VM.' ].
	[(Smalltalk vm interpreterSourceDate > '2012-07-08+2:00' asDate)
		ifFalse: displayError
	] on: Error do: [ :e| displayError value ].
]

{ #category : #current }
DiskStore class >> createDefault [
	^ self new
]

{ #category : #current }
DiskStore class >> current [
	^ self currentFileSystem store
]

{ #category : #current }
DiskStore class >> currentFileSystem [
	^ CurrentFS ifNil: [
		CurrentFS := FileSystem store: self activeClass createDefault]
]

{ #category : #accessing }
DiskStore class >> defaultWorkingDirectory [
	"Ask the VM for the default working directory.
	Clients should normally use the instance side method which caches this value."
	| pathString |

	pathString := OSPlatform current currentWorkingDirectoryPath.
	^(Path from: pathString delimiter: self delimiter).
]

{ #category : #public }
DiskStore class >> delimiter [
	^ self current delimiter
]

{ #category : #'class initialization' }
DiskStore class >> initialize [
	SessionManager default
		registerSystemClassNamed: self name
]

{ #category : #current }
DiskStore class >> isActiveClass [
	^ self delimiter = File delimiter
]

{ #category : #public }
DiskStore class >> maxFileNameLength [
	self subclassResponsibility 
]

{ #category : #current }
DiskStore class >> reset [
	DefaultWorkingDirectory := nil.
	CurrentFS := nil
]

{ #category : #'system startup' }
DiskStore class >> shutDown: quitting [
	"Force to detect filesystem after image restart"
	self reset
]

{ #category : #'system startup' }
DiskStore class >> startUp: resuming [
	self checkVMVersion.
	resuming 
		ifTrue: [ self reset ].
	DefaultWorkingDirectory := self defaultWorkingDirectory.
]

{ #category : #comparing }
DiskStore >> = other [
	^ self species = other species
]

{ #category : #accessing }
DiskStore >> accessTimeOf: aPath [
	"Return the date of last access of the File described by aPath"

	^DateAndTime fromSqueakTime: (File fileAttribute: (self stringFromPath: aPath) number: 9)
]

{ #category : #private }
DiskStore >> basenameFromEntry: entry [
	^ entry at: 1
]

{ #category : #private }
DiskStore >> basenameFromNode: aNode [
	^ aNode at: 1
]

{ #category : #private }
DiskStore >> basicCreationTimeOf: anEntry [
	" the entry contains the seconds since the squeak epoch in local time"
	^  (DateAndTime fromSeconds: (anEntry at: 2) offset: 0) translateTo: DateAndTime localOffset
]

{ #category : #private }
DiskStore >> basicEntry: directoryEntry path: aPath nodesDo: aBlock [
	| encodedPathString index entry pathString |
		
	index := 1.
	pathString := self stringFromPath: aPath.
	encodedPathString := File encodePathString: pathString.
	entry := File lookupEntryIn: encodedPathString index: index.
	entry = #badDirectoryPath ifTrue: [ ^ self signalDirectoryDoesNotExist: aPath ].
	
	[ entry isNil ]
		whileFalse: [ 
			entry at: 1 put: (File decodePathString: entry first).
			aBlock value: entry.
			index := index + 1.
			entry := File lookupEntryIn: encodedPathString index: index ].
]

{ #category : #public }
DiskStore >> basicEntryAt: aPath [
	| encodedPath encodedBasename |
	
	encodedPath := File encodePathString: (self stringFromPath: aPath parent).
	encodedBasename := File encodePathString: aPath basename.
	
	^ (File lookupDirectory: encodedPath filename: encodedBasename)
		ifNil: [ #badDirectoryPath ].
	
]

{ #category : #private }
DiskStore >> basicIsDirectory: aNode [
	| mask statAttributes |

	statAttributes := aNode at: 2.
	statAttributes ifNil: [ ^false ].
	mask := statAttributes at: 2.
	^(mask bitAnd: File s_IFMT) = File s_IFDIR
]

{ #category : #private }
DiskStore >> basicIsFile: anEntry [
	^ (anEntry at: 4) not
]

{ #category : #private }
DiskStore >> basicIsSymlink: anEntry [
	^(anEntry size >= 7)
		ifTrue: [ anEntry at: 7 ]
		ifFalse: [ false ]
]

{ #category : #private }
DiskStore >> basicModificationTimeOf: anEntry [
	" the entry contains the seconds since the squeak epoch in local time"
	^ (DateAndTime fromSeconds: (anEntry at: 3) offset: 0) translateTo: DateAndTime localOffset
]

{ #category : #public }
DiskStore >> basicOpen: aPath writable: aBoolean [
	| string encoded |
	string := self stringFromPath: aPath.
	encoded := File encodePathString: string.
	^ File open: encoded writable: aBoolean
]

{ #category : #private }
DiskStore >> basicPosixPermissions: anEntry [
	^ (anEntry size >= 6)
		ifTrue: [ anEntry at: 6 ]
		ifFalse: [ nil ].
]

{ #category : #private }
DiskStore >> basicSizeOf: anEntry [
	^ (anEntry at: 5)
]

{ #category : #accessing }
DiskStore >> changeTimeOf: aPath [
	"Answer the time the metadata of aPath was last changed.
	On platforms that don't support change time, use the modification time."

	| pathString squeakTime |
	
	pathString := self stringFromPath: aPath.
	squeakTime := (File fileAttribute: pathString number: 11) ifNil: 
			[ File fileAttribute: pathString number: 12 ].
	^DateAndTime fromSqueakTime: squeakTime
]

{ #category : #public }
DiskStore >> checkName: aFileName fixErrors: fixErrors [
	"Check a string aFileName for validity as a file name. Answer the original file name if it is valid. If the name is not valid (e.g., it is too long or contains illegal characters) and fixing is false, raise an error. If fixing is true, fix the name (usually by truncating and/or tranforming characters), and answer the corrected name. The default behavior is just to truncate the name to the maximum length for this platform. Subclasses can do any kind of checking and correction appropriate for their platform."
	
	| maxLength |
	aFileName size = 0 ifTrue: [self error: 'zero length file name'].
	maxLength := self maxFileNameLength.
	aFileName size > maxLength ifTrue: [
		fixErrors
			ifTrue: [^ aFileName contractTo: maxLength]
			ifFalse: [self error: 'file name is too long']].
	^ aFileName
]

{ #category : #public }
DiskStore >> createDirectory: path [
	"Create a directory for the argument path. 
	If the path refers to an existing file, raise FileExists.
	If the path refers to an existing directory, raise DirectoryExists.
	If the parent directory of the path does not exist, raise DirectoryDoesNotExist"

	| parent encodedPathString pathString result |
	pathString := self stringFromPath: path.
	encodedPathString := File encodePathString: pathString.
	result := File createDirectory: encodedPathString.
	result
		ifNil: [ 
			parent := path parent.
			(self exists: path)
				ifTrue: [ 
					(self isFile: path)
						ifTrue: [ self signalFileExists: path ]
						ifFalse: [ self signalDirectoryExists: path ] ].
			(self isDirectory: parent)
				ifFalse: [ ^ self signalDirectoryDoesNotExist: parent ].
			self primitiveFailed ].
	^ self
]

{ #category : #accessing }
DiskStore >> creationTimeOf: aPath [
	"Answer the creation time of aPath.  If the platform doesn't support creation time, use change time"
	| pathString squeakTime |

	pathString := self stringFromPath: aPath.
	squeakTime := (File fileAttribute: pathString number: 12) ifNil: 
			[ File fileAttribute: pathString number: 11 ].
	^DateAndTime fromSqueakTime: squeakTime

]

{ #category : #accessing }
DiskStore >> defaultWorkingDirectory [
	"Answer the default working directory, which is defined as the directory where the image resides."

	^ DefaultWorkingDirectory
		ifNil: [ self class defaultWorkingDirectory ]
]

{ #category : #public }
DiskStore >> delete: path [
	| pathString encodedPathString |
	
	((self exists: path) or: [ self isSymlink: path ])
		ifFalse: [ ^ FileDoesNotExistException signalWith: path ].
		
	pathString := self stringFromPath: path.
	encodedPathString := File encodePathString: pathString.
	
	(self isDirectory: path)
		ifTrue: [ File deleteDirectory: encodedPathString ]
		ifFalse: [ 
			(File named: pathString) delete ]
]

{ #category : #accessing }
DiskStore >> deviceIdOf: aPath [
	"Return the device id of the file at aPath"
	^File fileAttribute: (self stringFromPath: aPath) number: 4
]

{ #category : #public }
DiskStore >> entryFromNode: node path: path for: aFileSystem [

	| entryPath |
	entryPath := path / (self basenameFromNode: node).
	^DiskDirectoryEntry reference: (FileReference fileSystem: aFileSystem path: entryPath).

]

{ #category : #public }
DiskStore >> exists: aPath [

	aPath isRoot ifTrue: [ ^true ].
	^File exists: (self stringFromPath: aPath).
]

{ #category : #printing }
DiskStore >> forReferencePrintOn: aStream [
	aStream nextPutAll: 'File @ '
]

{ #category : #accessing }
DiskStore >> gidOf: aPath [
	"Return the gid of the File described by aPath"
	^File fileAttribute: (self stringFromPath: aPath) number: 7
]

{ #category : #accessing }
DiskStore >> handleClass [
	^ FileHandle
]

{ #category : #comparing }
DiskStore >> hash [
	^ self species hash
]

{ #category : #initialization }
DiskStore >> initialize [
	super initialize.
	maxFileNameLength := Smalltalk vm maxFilenameLength ifNil: [ 255 ].
]

{ #category : #testing }
DiskStore >> isBlock: aPath [
	"Answer a boolean indicating whether the supplied path is a Block file"
	^File isBlock: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isCharacter: aPath [
	"Answer a boolean indicating whether the supplied path is a Character file"
	^File isCharacter: (self stringFromPath: aPath)
]

{ #category : #accessing }
DiskStore >> isDirectory: aPath [
	"Answer a boolean indicating whether the supplied path is a Directory file"
	^aPath isRoot or: [File isDirectory: (self stringFromPath: aPath)]
]

{ #category : #testing }
DiskStore >> isDiskFileSystem [
	^ true
]

{ #category : #testing }
DiskStore >> isExecutable: aPath [

	^File isExecutable: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isFIFO: aPath [
	"Answer a boolean indicating whether the supplied path is a FIFO file (pipe)"
	^File isFIFO: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isFile: aPath [
	"Answer a boolean indicating whether the supplied path is a file, i.e. not a directory"

	aPath isRoot ifTrue: [ ^false ].
	^File isFile: (self stringFromPath: aPath)	
]

{ #category : #testing }
DiskStore >> isReadable: aPath [

	^File isReadable: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isRegular: aPath [
	"Answer a boolean indicating whether the supplied path is a Regular file"
	^File isRegular: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isSocket: aPath [
	"Answer a boolean indicating whether the supplied path is a Socket file"
	^File isSocket: (self stringFromPath: aPath)
]

{ #category : #public }
DiskStore >> isSymlink: aPath [
	| entry |
	
	aPath isRoot ifTrue: [ ^false ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsSymlink: entry ].
	
]

{ #category : #testing }
DiskStore >> isWritable: aPath [

	^File isWritable: (self stringFromPath: aPath)
]

{ #category : #public }
DiskStore >> maxFileNameLength [
	^ maxFileNameLength
]

{ #category : #accessing }
DiskStore >> modificationTimeOf: aPath [
	"Return the date of last modification of the File described by aPath"

	^DateAndTime fromSqueakTime: (File fileAttribute: (self stringFromPath: aPath) number: 10)
]

{ #category : #private }
DiskStore >> nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock [
	
	| entry |
	
	aPath isRoot ifTrue: [ ^ presentBlock value: self rootNode ].
	
	entry := self basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: absentBlock
		ifFalse: [
			entry at: 1 put: aPath basename.
			presentBlock value: entry ].
]

{ #category : #private }
DiskStore >> nodesAt: aPath do: aBlock [

	| pathString dirPointer entryData fileName attributes targetName |
	
	pathString := File encode: (self stringFromPath: aPath).
	(self isDirectory: aPath) ifFalse:
		[ ^self signalDirectoryDoesNotExist: aPath ].

	dirPointer := File primOpendir: pathString.
	dirPointer ifNil: [ ^nil ].
	[
		entryData := File primReaddir: dirPointer.
		[ entryData notNil ] whileTrue:
			[
				fileName := entryData first.
				entryData at: 1 put: (File decode: fileName).
				attributes := entryData at: 2.
				attributes ifNotNil: 
					[ targetName := attributes at: 1.
					targetName ifNotNil: [ attributes at: 1 put: (File decode: targetName) ] ].
				aBlock value: entryData.
				entryData := File primReaddir: dirPointer.
			]
	] ensure: [ File primClosedir: dirPointer ].
]

{ #category : #accessing }
DiskStore >> numberOfHardLinks: aPath [
	"Return the number of hard links for the File described by aPath"
	^File fileAttribute: (self stringFromPath: aPath) number: 5
]

{ #category : #public }
DiskStore >> openFileStream: path writable: writable [
	| fullPath |
	fullPath := self stringFromPath: path.
	"redirect over the default implementation"
	^ writable 
		ifFalse: [ FileStream readOnlyFileNamed: fullPath ]
		ifTrue: [ FileStream fileNamed: fullPath ]
]

{ #category : #accessing }
DiskStore >> permissions: aPath [

	^File permissions: (self stringFromPath: aPath)
]

{ #category : #public }
DiskStore >> rename: sourcePath to: destinationPath [

	| sourcePathString encodedSourcePathString targetPathString encodedTargetPathString |
	sourcePathString := self stringFromPath: sourcePath.
	encodedSourcePathString := File encodePathString: sourcePathString.
	targetPathString := self stringFromPath: destinationPath.
	encodedTargetPathString := File encodePathString: targetPathString.
	^ File rename: encodedSourcePathString to: encodedTargetPathString.
]

{ #category : #private }
DiskStore >> rootNode [
	^ #('' 0 0 true 0 8r555)
]

{ #category : #accessing }
DiskStore >> sizeOf: aPath [
	"Return the size of the File described by aPath"
	^File fileAttribute: (self stringFromPath: aPath) number: 8.
]

{ #category : #accessing }
DiskStore >> uidOf: aPath [
	"Return the uid of the File described by aPath"
	^File fileAttribute: (self stringFromPath: aPath) number: 6
]
