"
SUnit tests for the disk filesystem
"
Class {
	#name : #DiskFileSystemTest,
	#superclass : #FileSystemTest,
	#category : #'FileSystem-Tests-Disk-Base'
}

{ #category : #initialization }
DiskFileSystemTest >> createFileSystem [
	^ FileSystem store: (DiskStore activeClass createDefault)
]

{ #category : #tests }
DiskFileSystemTest >> testDefaultWorkingDirectory [
	| ref |
	ref := filesystem workingDirectory.
	self assert: ('File @ ' , OSPlatform current currentWorkingDirectoryPath beginsWith: ref asString)
]

{ #category : #tests }
DiskFileSystemTest >> testEntriesHaveAttributes [
	"Disk entries returned by FileReference>>entries should be pre-loaded with statAttributes"

	| entries |

	entries := FileLocator imageDirectory resolve entries.
	"Ensure that we are actually checking entries"
	self assert: entries isNotEmpty.
	"We can't use the accessor method to get the statAttributes since does lazy initialisation"
	self assert: (entries select: [ :each | (each instVarNamed: 'statAttributes') isNil ]) isEmpty.
]

{ #category : #tests }
DiskFileSystemTest >> testEqual [
	| other |
	other := self createFileSystem.
	self assert: filesystem equals: other
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
DiskFileSystemTest >> testLongFilename [
	"Test that long file names (in Win32 terms, i.e. < 260 characters) are working"

	| dir1 dir2 file contents |

	dir1 := FileLocator workingDirectory / (String new: 130 withAll: $a).
	[ self deny: dir1 exists.
	dir1 ensureCreateDirectory.

	dir2 := dir1 / (String new: 130 withAll: $b).
	self deny: dir2 exists.
	dir2 ensureCreateDirectory.

	file := dir2 / 'hello.txt'.
	self deny: file exists.
	contents := 'hello world'.
	file writeStreamDo: [ :stream | stream << contents ].

	self assert: file exists.
	self assert: file contents equals: contents ]
		ensure: [ dir1 ensureDeleteAll ]
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
