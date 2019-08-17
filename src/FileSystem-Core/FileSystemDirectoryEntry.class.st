"
I am a cache for metadata about a file, symbolic link or directory.

The set of attributes supported is platform dependent.  See the class comments of my subclasses for the details.
"
Class {
	#name : #FileSystemDirectoryEntry,
	#superclass : #Object,
	#instVars : [
		'reference'
	],
	#category : #'FileSystem-Core-Public'
}

{ #category : #accessing }
FileSystemDirectoryEntry class >> allPosixPermissions [ 
	^8r777
]

{ #category : #'instance creation' }
FileSystemDirectoryEntry class >> fileSystem: aFilesystem path: aPath [
	"Create a directory entry given a filesystem and a path in such filesystem.
	This method is here for backward compatibility, the appropriate subclass should be called directly, and not rely on the switch statement here."

	^ aFilesystem store isDiskFileSystem ifTrue: 
		[ DiskDirectoryEntry reference: (aFilesystem referenceTo: aPath) ]
	ifFalse:
		[ MemoryDirectoryEntry reference: (aFilesystem referenceTo: aPath) ]
]

{ #category : #'instance creation' }
FileSystemDirectoryEntry class >> fileSystem: aFilesystem path: aPath creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBooleam [
	"Create a directory entry given a filesystem and a path in such filesystem.
	This method is here for backward compatibility.  All other file attributes are retrieved independently."

	^ aFilesystem store isDiskFileSystem ifTrue: 
		[ DiskDirectoryEntry reference: (aFilesystem referenceTo: aPath) ]
	ifFalse:
		[ MemoryDirectoryEntry reference: (aFilesystem referenceTo: aPath) ]
]

{ #category : #'instance creation' }
FileSystemDirectoryEntry class >> reference: aFileReference [

	^self new initializeWithReference: aFileReference
]

{ #category : #'instance creation' }
FileSystemDirectoryEntry class >> reference: ref creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBoolean [
	"Create a directory entry given a file reference.
	This method is here for backward compatibility.  All other file attributes are retrieved independently."

	^ ref store isDiskFileSystem ifTrue: 
		[ DiskDirectoryEntry reference: ref ]
	ifFalse:
		[ MemoryDirectoryEntry reference: ref ]
]

{ #category : #accessing }
FileSystemDirectoryEntry >> accessTime [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #converting }
FileSystemDirectoryEntry >> asFileReference [
	^ reference
]

{ #category : #accessing }
FileSystemDirectoryEntry >> basename [

	^ reference basename
]

{ #category : #accessing }
FileSystemDirectoryEntry >> changeTime [
	"Answer the files change time"

	^FileAttributeNotSupported signalWith: self
]

{ #category : #accessing }
FileSystemDirectoryEntry >> creation [
	^ self creationTime 
]

{ #category : #accessing }
FileSystemDirectoryEntry >> creationTime [
	"Answer the files creation time.
	Different platforms will interpret this in different ways."

	^FileAttributeNotSupported signalWith: self
]

{ #category : #accessing }
FileSystemDirectoryEntry >> deviceId [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #testing }
FileSystemDirectoryEntry >> exists [
	"Answer a boolean indicating whether the receivers file exists"

	^self reference exists
]

{ #category : #delegate }
FileSystemDirectoryEntry >> extension [
	^ reference extension
]

{ #category : #delegate }
FileSystemDirectoryEntry >> fullName [
	^ reference fullName
]

{ #category : #'cache management' }
FileSystemDirectoryEntry >> getAccessAttributes [
	"Noop by default"
	
	^self
]

{ #category : #'cache management' }
FileSystemDirectoryEntry >> getAllAttributes [
	"Noop by default"
	
	^self
]

{ #category : #'cache management' }
FileSystemDirectoryEntry >> getStatAttributes [
	"Noop by default"
	
	^self
]

{ #category : #accessing }
FileSystemDirectoryEntry >> gid [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #initialization }
FileSystemDirectoryEntry >> initializeWithReference: ref [ 

	super initialize.	
	reference := ref.

]

{ #category : #accessing }
FileSystemDirectoryEntry >> inode [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #testing }
FileSystemDirectoryEntry >> isBlock [

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isCharacter [

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isDirectory [
	"Return whether the receiver is a directory"

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isExecutable [

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isFIFO [

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isFile [

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isHidden [
	"Return whether the receiver is hidden"

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isReadable [

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isRegular [

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isSocket [

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isSymlink [

	^false
]

{ #category : #testing }
FileSystemDirectoryEntry >> isWritable [

	^false
]

{ #category : #accessing }
FileSystemDirectoryEntry >> mode [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #accessing }
FileSystemDirectoryEntry >> modification [
	^ self modificationTime 
]

{ #category : #accessing }
FileSystemDirectoryEntry >> modificationTime [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #accessing }
FileSystemDirectoryEntry >> numberOfHardLinks [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #delegate }
FileSystemDirectoryEntry >> pathSegments [
	^ reference pathSegments
]

{ #category : #delegate }
FileSystemDirectoryEntry >> pathString [
	"Answer the full path name of the receiver"

	^reference pathString
]

{ #category : #accessing }
FileSystemDirectoryEntry >> permissions [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #accessing }
FileSystemDirectoryEntry >> permissions: permissions [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #printing }
FileSystemDirectoryEntry >> printAttribute: anAttribute on: aStream [

	[ (anAttribute value: self) printOn: aStream ]
		on: FileAttributeNotSupported 
		do: [ aStream << '(not supported)' ]
]

{ #category : #printing }
FileSystemDirectoryEntry >> printAttributeNamed: attributeName on: aStream [

	aStream
		tab; 
		<< attributeName;
		<< ':';
		tab.
	self printAttribute: attributeName on: aStream.
	aStream cr.

]

{ #category : #printing }
FileSystemDirectoryEntry >> printLongFormOn: aStream [

	reference path printOn: aStream.
	aStream cr.
	self
		printAttributeNamed: #accessTime on: aStream;
		printAttributeNamed: #modificationTime on: aStream;
		printAttributeNamed: #changeTime on: aStream;
		printAttributeNamed: #creationTime on: aStream;
		printAttributeNamed: #deviceId on: aStream;
		printAttributeNamed: #inode on: aStream;
		printAttributeNamed: #fileSize on: aStream;
		printAttributeNamed: #uid on: aStream;
		printAttributeNamed: #gid on: aStream;
		printAttributeNamed: #numberOfHardLinks on: aStream;
		printAttributeNamed: #permissions on: aStream;
		printAttributeNamed: #targetPath on: aStream;
		printAttributeNamed: #isBlock on: aStream;
		printAttributeNamed: #isCharacter on: aStream;
		printAttributeNamed: #isDirectory on: aStream;
		printAttributeNamed: #isExecutable on: aStream;
		printAttributeNamed: #isFIFO on: aStream;
		printAttributeNamed: #isFile on: aStream;
		printAttributeNamed: #isReadable on: aStream;
		printAttributeNamed: #isSocket on: aStream;
		printAttributeNamed: #isSymlink on: aStream;
		printAttributeNamed: #isWritable on: aStream.
]

{ #category : #printing }
FileSystemDirectoryEntry >> printOn: aStream [

	aStream nextPutAll: 'DirectoryEntry: '.
	reference ifNotNil: [:ref | aStream nextPutAll: reference printString].
]

{ #category : #delegate }
FileSystemDirectoryEntry >> readStream [
	^ reference readStream
]

{ #category : #accessing }
FileSystemDirectoryEntry >> reference [
	^ reference
]

{ #category : #accessing }
FileSystemDirectoryEntry >> size [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #accessing }
FileSystemDirectoryEntry >> targetFileReference [
	"Answer a FileReference representing the receivers target file"
	
	^FileReference fileSystem: reference fileSystem path: self targetPath.
]

{ #category : #accessing }
FileSystemDirectoryEntry >> targetPath [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #accessing }
FileSystemDirectoryEntry >> uid [

	^FileAttributeNotSupported signalWith: self
]

{ #category : #delegate }
FileSystemDirectoryEntry >> writeStream [
	^ reference writeStream
]
