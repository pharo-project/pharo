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
		'DefaultWorkingDirectory',
		'FileAttributePrimitives',
		'Primitives'
	],
	#category : #'FileSystem-Disk'
}

{ #category : #current }
DiskStore class >> activeClass [
	self allSubclassesDo: [:ea | 
		ea isActiveClass ifTrue: [^ ea]].
	^ self
]

{ #category : #accessing }
DiskStore class >> attributesClass [

	^DiskFileAttributes 
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
	self useFilePlugin.
	SessionManager default
		registerSystemClassNamed: self name
]

{ #category : #current }
DiskStore class >> isActiveClass [
	^ self delimiter = Primitives delimiter
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

{ #category : #'class initialization' }
DiskStore class >> useFilePlugin [
	"Initialise the plugins required by the store and associated directory entries."
	Primitives := FilePluginPrims new.
	"FileAttributePrimitives is set by the appropriate subclass"
	FileAttributePrimitives class reset.
	DiskDirectoryEntry initializeWithPrimitives: FileAttributePrimitives.
]

{ #category : #comparing }
DiskStore >> = other [
	^ self species = other species
]

{ #category : #accessing }
DiskStore >> accessTimeOf: aPath [
	"Return the date of last access of the File described by aPath"
	^DateAndTime fromUnixTime:
		(FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 9).
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
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^  (DateAndTime fromSeconds: (anEntry at: 2) offset: 0) translateTo: DateAndTime localOffset
]

{ #category : #private }
DiskStore >> basicEntry: directoryEntry path: aPath nodesDo: aBlock [
	| encodedPathString index entry pathString |

	self 
		deprecated: 'Use #nodesAt:do: instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	index := 1.
	pathString := self stringFromPath: aPath.
	encodedPathString := Primitives encode: pathString.
	entry := Primitives lookupEntryIn: encodedPathString index: index.
	entry = #badDirectoryPath ifTrue: [ ^ self signalDirectoryDoesNotExist: aPath ].
	
	[ entry isNil ]
		whileFalse: [ 
			entry at: 1 put: (Primitives decode: entry first).
			aBlock value: entry.
			index := index + 1.
			entry := Primitives lookupEntryIn: encodedPathString index: index ].
]

{ #category : #public }
DiskStore >> basicEntryAt: aPath [
	| encodedPath encodedBasename |
	
	encodedPath := Primitives encode: (self stringFromPath: aPath parent).
	encodedBasename := Primitives encode: aPath basename.
	
	^ (Primitives lookupDirectory: encodedPath filename: encodedBasename)
		ifNil: [ #badDirectoryPath ].
	
]

{ #category : #private }
DiskStore >> basicIsDirectory: aNode [
	| mask statAttributes |

	statAttributes := aNode at: 2.
	statAttributes ifNil: [ ^false ].
	mask := statAttributes at: 2.
	^(mask bitAnd: self fileAttributePrimitives s_IFMT) = self fileAttributePrimitives s_IFDIR
]

{ #category : #private }
DiskStore >> basicIsFile: anEntry [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (anEntry at: 4) not
]

{ #category : #private }
DiskStore >> basicIsSymlink: anEntry [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^(anEntry size >= 7)
		ifTrue: [ anEntry at: 7 ]
		ifFalse: [ false ]
]

{ #category : #private }
DiskStore >> basicModificationTimeOf: anEntry [
	" the entry contains the seconds since the squeak epoch in local time"
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (DateAndTime fromSeconds: (anEntry at: 3) offset: 0) translateTo: DateAndTime localOffset
]

{ #category : #public }
DiskStore >> basicOpen: aPath writable: aBoolean [
	| string encoded |
	string := self stringFromPath: aPath.
	encoded := Primitives encode: string.
	^ Primitives open: encoded writable: aBoolean
]

{ #category : #private }
DiskStore >> basicPosixPermissions: anEntry [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (anEntry size >= 6)
		ifTrue: [ anEntry at: 6 ]
		ifFalse: [ nil ].
]

{ #category : #private }
DiskStore >> basicSizeOf: anEntry [
	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.
	^ (anEntry at: 5)
]

{ #category : #accessing }
DiskStore >> changeTimeOf: aPath [

	^DateAndTime fromUnixTime:
		(FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 11).
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
	encodedPathString := Primitives encode: pathString.
	result := Primitives createDirectory: encodedPathString.
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

	| pathString unixTime |
	
	pathString := self stringFromPath: aPath.
	"FileAttributesPluginPrims>>fileAttribute:number: v1.2.6 and earlier has a bug that results in a creationTime returning -1 instead of nil.  For now, treat -1 the same as nil"
	unixTime := FileAttributePrimitives fileAttribute: pathString number: 12.
	(unixTime isNil or: [ unixTime = -1 ]) ifTrue: 
		[ unixTime := FileAttributePrimitives fileAttribute: pathString number: 11 ].
	^DateAndTime fromUnixTime: unixTime.
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
		ifFalse: [ ^ FileDoesNotExist signalWith: path ].
		
	pathString := self stringFromPath: path.
	encodedPathString := Primitives encode: pathString.
	
	(self isDirectory: path)
		ifTrue: [ Primitives deleteDirectory: encodedPathString ]
		ifFalse: [ 
			StandardFileStream
				retryWithGC: [ Primitives deleteFile: encodedPathString ]
				until: [ :result | result notNil ]
				forFileNamed: pathString ]
]

{ #category : #accessing }
DiskStore >> deviceIdOf: aPath [
	"Return the device id of the File described by aPath"
	^ FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 4
]

{ #category : #private }
DiskStore >> entryFor: aPath [
	"Answer the Directory Entry for the supplied path"

	^DiskDirectoryEntry reference: (FileReference fileSystem: FileSystem disk path: aPath)
]

{ #category : #public }
DiskStore >> entryFromNode: node path: path for: aFileSystem [

	| entryPath |
	entryPath := path / (self basenameFromNode: node).
	^DiskDirectoryEntry reference: (FileReference fileSystem: aFileSystem path: entryPath).

]

{ #category : #testing }
DiskStore >> exists: aPath [

	aPath isRoot ifTrue: [ ^true ].
	^self fileAttributePrimitives exists: (self stringFromPath: aPath).
]

{ #category : #private }
DiskStore >> fileAttributePrimitives [
	"Answer the primitives for file attribute and directory enumerating operations."

	^FileAttributePrimitives
]

{ #category : #printing }
DiskStore >> forReferencePrintOn: aStream [
	aStream nextPutAll: 'File @ '
]

{ #category : #accessing }
DiskStore >> gidOf: aPath [
	"Return the gid of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 7
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

{ #category : #accessing }
DiskStore >> inodeOf: aPath [
	"Return the inode number of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 3
]

{ #category : #testing }
DiskStore >> isBlock: aPath [
	"Answer a boolean indicating whether the supplied path is a Block file"
	^FileAttributePrimitives isBlock: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isCharacter: aPath [
	"Answer a boolean indicating whether the supplied path is a Character file"
	^FileAttributePrimitives isCharacter: (self stringFromPath: aPath)
]

{ #category : #accessing }
DiskStore >> isDirectory: aPath [
	"Answer a boolean indicating whether the supplied path is a Directory file"
	^FileAttributePrimitives isDirectory: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isDiskFileSystem [
	^ true
]

{ #category : #public }
DiskStore >> isExecutable: aPath [

	^FileAttributePrimitives isExecutable: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isFIFO: aPath [
	"Answer a boolean indicating whether the supplied path is a FIFO file"
	^FileAttributePrimitives isFIFO: (self stringFromPath: aPath)
]

{ #category : #public }
DiskStore >> isFile: aPath [
	"Answer a boolean indicating whether the supplied path is a file, i.e. not a directory"

	aPath isRoot ifTrue: [ ^false ].
	^FileAttributePrimitives isFile: (self stringFromPath: aPath)	
]

{ #category : #public }
DiskStore >> isReadable: aPath [

	^FileAttributePrimitives isReadable: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isRegular: aPath [
	"Answer a boolean indicating whether the supplied path is a Regular file"
	^FileAttributePrimitives isRegular: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isSocket: aPath [
	"Answer a boolean indicating whether the supplied path is a Socket file"
	^FileAttributePrimitives isSocket: (self stringFromPath: aPath)
]

{ #category : #testing }
DiskStore >> isSymlink: aPath [
	"Answer a boolean indicating whether the supplied path is a Symlink file"
	^FileAttributePrimitives isSymlink: (self stringFromPath: aPath)
]

{ #category : #public }
DiskStore >> isWritable: aPath [

	^FileAttributePrimitives isWritable: (self stringFromPath: aPath)
]

{ #category : #public }
DiskStore >> maxFileNameLength [
	^ maxFileNameLength
]

{ #category : #private }
DiskStore >> modeOf: aPath [
	"Answer the statBuf.st_mode for the supplied path"
	
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 2
]

{ #category : #accessing }
DiskStore >> modificationTimeOf: aPath [
	"Return the date of last access of the File described by aPath"
	^DateAndTime fromUnixTime:
		(FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 10).
]

{ #category : #private }
DiskStore >> nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock [
	
	| entry |

	self 
		deprecated: 'Use FileAttributes instead'
		on: 	'30 Apr 2017' 
		in: 'Pharo-7.0-hopefully-'.

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
	
	pathString := Primitives encode: (self stringFromPath: aPath).
	(self isDirectory: aPath) ifFalse:
		[ ^self signalDirectoryDoesNotExist: aPath ].

	dirPointer := FileAttributePrimitives primOpendir: pathString.
	dirPointer ifNil: [ ^nil ].
	[
		entryData := FileAttributePrimitives primReaddir: dirPointer.
		[ entryData notNil ] whileTrue:
			[
				fileName := entryData first.
				entryData at: 1 put: (FileAttributePrimitives decode: fileName).
				attributes := entryData at: 2.
				attributes ifNotNil: 
					[ targetName := attributes at: 1.
					targetName ifNotNil: [ attributes at: 1 put: (Primitives decode: targetName) ] ].
				aBlock value: entryData.
				entryData := FileAttributePrimitives primReaddir: dirPointer.
			]
	] ensure: [ FileAttributePrimitives primClosedir: dirPointer ].
]

{ #category : #accessing }
DiskStore >> numberOfHardLinks: aPath [
	"Return the number of hard links for the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 5
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
	^FileAttributePrimitives permissions: (self stringFromPath: aPath)
]

{ #category : #public }
DiskStore >> rename: sourcePath to: destinationPath [

	| sourcePathString encodedSourcePathString targetPathString encodedTargetPathString |
	sourcePathString := self stringFromPath: sourcePath.
	encodedSourcePathString := Primitives encode: sourcePathString.
	targetPathString := self stringFromPath: destinationPath.
	encodedTargetPathString := Primitives encode: targetPathString.
	^ Primitives rename: encodedSourcePathString to: encodedTargetPathString.
]

{ #category : #private }
DiskStore >> rootNode [
	^ #('' 0 0 true 0 8r555)
]

{ #category : #accessing }
DiskStore >> sizeOf: aPath [
	"Return the size of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 8.
]

{ #category : #private }
DiskStore >> statAttributesFromNode: aNode [
	"Answer the stat attributes array from the supplied node"
	
	^aNode at: 2
]

{ #category : #accessing }
DiskStore >> uid: aPath [
	"Return the uid of the File described by aPath"
	^FileAttributePrimitives fileAttribute: (self stringFromPath: aPath) number: 6.
]
