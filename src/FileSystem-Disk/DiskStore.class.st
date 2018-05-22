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

{ #category : #private }
DiskStore >> basenameFromEntry: entry [
	^ entry at: 1
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
DiskStore >> basicIsDirectory: anEntry [
	^ anEntry at: 4
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
DiskStore >> defaultWorkingDirectory [
	"Answer the default working directory, which is defined as the directory where the image resides."

	^ DefaultWorkingDirectory
		ifNil: [ self class defaultWorkingDirectory ]
]

{ #category : #public }
DiskStore >> delete: path [
	| pathString encodedPathString |
	
	(self exists: path)
		ifFalse: [ ^ FileDoesNotExist signalWith: path ].
		
	pathString := self stringFromPath: path.
	encodedPathString := File encodePathString: pathString.
	
	(self isDirectory: path)
		ifTrue: [ File deleteDirectory: encodedPathString ]
		ifFalse: [ 
			(File named: pathString) delete ]
]

{ #category : #printing }
DiskStore >> forReferencePrintOn: aStream [
	aStream nextPutAll: 'File @ '
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

{ #category : #public }
DiskStore >> isDirectory: aPath [
	| entry |
	
	aPath isRoot ifTrue: [ ^ true ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsDirectory: entry ].
	
]

{ #category : #testing }
DiskStore >> isDiskFileSystem [
	^ true
]

{ #category : #public }
DiskStore >> isFile: aPath [
	| entry |
	
	aPath isRoot ifTrue: [ ^ false ].
	
	entry := self  basicEntryAt: aPath.
	
	^ entry == #badDirectoryPath 
		ifTrue: [  false ]
		ifFalse: [ self basicIsFile: entry ].
	
]

{ #category : #public }
DiskStore >> isReadable: aPath [
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath writable: false) 
		ifNotNil: [ :id|
			File close: id.
			^ true].
	^ false
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

{ #category : #public }
DiskStore >> isWritable: aPath [
	(self exists: aPath) ifFalse: [ ^ false ].
	self flag: 'TODO: we need a decent primitive for this...'.
	(self basicOpen: aPath writable: true) 
		ifNotNil: [ :id|
			File close: id.
			^ true].
	^ false
]

{ #category : #public }
DiskStore >> maxFileNameLength [
	^ maxFileNameLength
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

{ #category : #public }
DiskStore >> openFileStream: path writable: writable [
	| fullPath |
	fullPath := self stringFromPath: path.
	"redirect over the default implementation"
	^ writable 
		ifFalse: [ FileStream readOnlyFileNamed: fullPath ]
		ifTrue: [ FileStream fileNamed: fullPath ]
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
