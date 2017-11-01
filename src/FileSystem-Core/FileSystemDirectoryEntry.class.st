"
I am a cache for metadata about a file or directory. The information I hold is as follows:

reference
	A reference to the file or directory to which my data pertains.
	
creation
	The creation date and time, stored as number seconds since the 
	Smalltalk epoch.
	
modification
	The modification date and time, number seconds since the Smalltalk epoch.
	
isDirectory 
	True if my data pertains to a directory, false if a file.
	
size
	Size in bytes for a file, 0 for a directory.

"
Class {
	#name : #FileSystemDirectoryEntry,
	#superclass : #Object,
	#instVars : [
		'reference',
		'creation',
		'modification',
		'isDirectory',
		'isSymlink',
		'size',
		'posixPermissions'
	],
	#category : 'FileSystem-Core-Public'
}

{ #category : #accessing }
FileSystemDirectoryEntry class >> allPosixPermissions [ 
	^8r777
]

{ #category : #'instance creation' }
FileSystemDirectoryEntry class >> fileSystem: aFilesystem path: aPath creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBooleam [
	"Create a directory entry given a filesystem and a path in such filesystem. In addition, the creation and modification time are required as well as a boolean that indicates whether the entry is a folder or a file and its size."

	^ self 
		reference: (aFilesystem referenceTo: aPath)
		creation: cTime
		modification: mTime
		isDir: aBoolean
		size: anInteger
		posixPermissions: posixNumber
		isSymlink: symlinkBooleam
]

{ #category : #'instance creation' }
FileSystemDirectoryEntry class >> reference: ref creation: cTime modification: mTime isDir: aBoolean size: anInteger posixPermissions: posixNumber isSymlink: symlinkBoolean [
	"Create a directory entry for the file reference ref, with the creation time, cTime, the modification time, mTime. aBoolean indicates if the entry represents a directory or a file of size given by anInteger"
	
	^ self basicNew
		initializeWithRef: ref
		creation: cTime
		modification: mTime
		isDir: aBoolean
		size: anInteger
		posixPermissions: posixNumber
		isSymlink: symlinkBoolean
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
FileSystemDirectoryEntry >> creation [
	^ self creationTime 
]

{ #category : #accessing }
FileSystemDirectoryEntry >> creationSeconds [
	"Return the creation date and time of the entry receiver in seconds."
		
	^ creation asSeconds
]

{ #category : #accessing }
FileSystemDirectoryEntry >> creationTime [
	"Return the creation date and time of the entry receiver."
	
	^ creation
]

{ #category : #delegate }
FileSystemDirectoryEntry >> extension [
	^ reference extension
]

{ #category : #delegate }
FileSystemDirectoryEntry >> fullName [
	^ reference fullName
]

{ #category : #initialize-release }
FileSystemDirectoryEntry >> initializeWithRef: ref 
	creation: cTime 
	modification: mTime 
	isDir: directoryBoolean
	size: bytes 
	posixPermissions: posixNumber 
	isSymlink: symlinkBoolean [
	
	reference := ref.
	creation := cTime.
	modification := mTime.
	isDirectory := directoryBoolean.
	size := bytes.
	posixPermissions := posixNumber.
	isSymlink := symlinkBoolean.
]

{ #category : #testing }
FileSystemDirectoryEntry >> isDirectory [
	"Return whether the receiver is a directory"
	
	^ isDirectory
]

{ #category : #testing }
FileSystemDirectoryEntry >> isFile [
	"Return whether the receiver is a file"
	
	^ isDirectory not
]

{ #category : #testing }
FileSystemDirectoryEntry >> isSymlink [ 
	^isSymlink
]

{ #category : #accessing }
FileSystemDirectoryEntry >> modification [
	^ self modificationTime 
]

{ #category : #accessing }
FileSystemDirectoryEntry >> modificationSeconds [
	"Return the modification date and time of the entry receiver in seconds."
	^ modification
]

{ #category : #accessing }
FileSystemDirectoryEntry >> modificationTime [
	"Return the modification date and time of the entry receiver."

	^  modification
]

{ #category : #delegate }
FileSystemDirectoryEntry >> pathSegments [
	^ reference pathSegments
]

{ #category : #accessing }
FileSystemDirectoryEntry >> permissions [ 
	^self posixPermissions
		ifNotNil: [ FileSystemPermission posixPermissions: self posixPermissions ]
]

{ #category : #accessing }
FileSystemDirectoryEntry >> posixPermissions [ 
	^posixPermissions
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
	"Returns the receiver size"
	^ size
]

{ #category : #delegate }
FileSystemDirectoryEntry >> writeStream [
	^ reference writeStream
]
