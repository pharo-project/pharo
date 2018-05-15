"
SUnit tests for the disk filesystem
"
Class {
	#name : #DiskFileSystemTest,
	#superclass : #FileSystemTest,
	#category : #'FileSystem-Tests-Disk-Base'
}

{ #category : #'initialize-release' }
DiskFileSystemTest >> createFileSystem [
	^ FileSystem store: (DiskStore activeClass createDefault)
]

{ #category : #tests }
DiskFileSystemTest >> testDefaultWorkingDirectory [
	| ref |
	ref := filesystem workingDirectory.
	self assert: (('File @ ', Smalltalk vm imagePath) beginsWith: ref asString)
]

{ #category : #tests }
DiskFileSystemTest >> testEqual [
	| other |
	other := self createFileSystem.
	self assert: filesystem = other
]

{ #category : #tests }
DiskFileSystemTest >> testIsDirectory [
	self assert: (filesystem isDirectory: FileLocator imageDirectory resolve path)
]

{ #category : #tests }
DiskFileSystemTest >> testIsDiskFileSystem [
	self assert: filesystem isDiskFileSystem.
	
]

{ #category : #tests }
DiskFileSystemTest >> testMoveMemoryToDisk [
	"Test moving a file from the memory filesystem to the disk filesystem.
	This ensures that the copyAndDelete:to: is called correctly."
	| memfs out testString old new |
	[
		memfs := FileSystem memory.
		old := memfs / 'testMoveMemoryToDisk_old'.
		out := old writeStream.
		testString := 'To be moved to disk'.
		[ out nextPutAll: testString ] ensure: [ out close ].
		
		new := FileLocator imageDirectory / 'testMoveMemoryToDisk_new'.
		old moveTo: new.
		
		self deny: (memfs / 'testMoveMemoryToDisk_old') exists.
		self assert: new exists.
		self assert: new contents equals: testString.
	] ensure: [ 
		old ensureDelete.
		new ensureDelete.
	]
]
