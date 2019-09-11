"
SUnit tests for MemoryFileSystem
"
Class {
	#name : #MemoryFileSystemTest,
	#superclass : #FileSystemTest,
	#category : #'FileSystem-Tests-Memory-Base'
}

{ #category : #initialization }
MemoryFileSystemTest >> createFileSystem [
	^ FileSystem memory
]

{ #category : #private }
MemoryFileSystemTest >> createFilesIn: root from: anArray [
	"Helper method to create a directory hierarchy with files."

	| dir path |

	dir := (root / anArray first) createDirectory.
	anArray allButFirstDo: [ :each |
		each isArray ifTrue: 
			[ self createFilesIn: dir from: each ]
		ifFalse: 
			[ (path := dir / each) writeStreamDo: [ :stream |
				stream nextPutAll: path pathString ] ] ]
]

{ #category : #tests }
MemoryFileSystemTest >> lastModificationTimeOf: fileReference [
	"DateAndTime primUTCMicrosecondsClock is not precise across all OS so put in slight delay between calling modificationTime"

	^ [ fileReference modificationTime ]
		ensure: [ (Delay forMilliseconds: 100) wait ]
]

{ #category : #tests }
MemoryFileSystemTest >> testCopyFromto [

	| fileReference buffer |

	fileReference := filesystem root / 'test.file'.
	buffer := 'test data'.
	filesystem
		copyFrom: buffer readStream 
		to: fileReference path.
	self assert: fileReference contents equals: buffer.
]

{ #category : #tests }
MemoryFileSystemTest >> testCurrentEqual [
	| other another |
	another := FileSystem currentMemoryFileSystem.
	other := FileSystem currentMemoryFileSystem.
	self assert: other equals: another
]

{ #category : #tests }
MemoryFileSystemTest >> testDirectoryMoveTo [
	"Test the scenario where the source is a directory, the destination is an existing directory and they are on separate file systems."
	| srcFS destFS src dest count f33 |

	srcFS := filesystem workingDirectory.
	destFS := FileSystem memory workingDirectory.
	self createFilesIn: srcFS from: #(
		src
		f11
		f12
		#(
			d11
			f21
			f22
			#(
				d21
				f31
				f32)
			#(
				d22
				f33
				f34))).

	[src := srcFS / 'src'.
	count := src allChildren size.
	dest := destFS / 'dest'.
	dest ensureCreateDirectory.
	f33 := dest / 'src/d11/d22/f33'.
	src copy moveTo: dest.
	self deny: src exists.
	self assert: (dest / 'src') allChildren size equals: count.
	self assert: f33 exists.
	self assert: f33 contents equals: '/src/d11/d22/f33']
		ensure: 
			[ src exists ifTrue: [ src deleteAll ].
			dest exists ifTrue: [ dest deleteAll ] ].
]

{ #category : #tests }
MemoryFileSystemTest >> testDirectoryMoveToRename [
	"Test the scenario where the source is a directory, the destination doesn't exist (its parent does) and they are on separate file systems, i.e. move and rename the source to the destination."
	| srcFS destFS src dest count f33 |

	srcFS := filesystem workingDirectory.
	destFS := FileSystem memory workingDirectory.
	self createFilesIn: srcFS from: #(
		src
		f11
		f12
		#(
			d11
			f21
			f22
			#(
				d21
				f31
				f32)
			#(
				d22
				f33
				f34))).

	[src := srcFS / 'src'.
	count := src allChildren size.
	dest := (destFS / 'dest') ensureCreateDirectory / 'renamed'.
	f33 := dest / 'd11/d22/f33'.
	src copy moveTo: dest.
	self deny: src exists.
	self assert: dest allChildren size equals: count.
	self assert: f33 exists.
	self assert: f33 contents equals: '/src/d11/d22/f33']
		ensure: 
			[ src exists ifTrue: [ src deleteAll ].
			dest exists ifTrue: [ dest deleteAll ] ].
]

{ #category : #tests }
MemoryFileSystemTest >> testEnsureCreateFileDoesNotOverwriteExistingContent [
	| file |
	file := filesystem root / 'a.txt'.
	file writeStreamDo: [ :s | s nextPutAll: 'ok' ].
	self assert: file contents equals: 'ok'.
	
	file ensureCreateFile.

	self assert: file contents equals: 'ok'.

]

{ #category : #tests }
MemoryFileSystemTest >> testIsMemoryFileSystem [
	self assert: filesystem isMemoryFileSystem.
	
]

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenFileCreated [
	self assert: (filesystem / 'file.txt') ensureCreateFile modificationTime notNil
]

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenFileModifiedByWriteStream [
	| modifiedTime fileReference |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	fileReference writeStreamDo: [ :aStream | aStream nextPutAll: 'data' ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
]

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenFileModifiedWithBinaryWriteStream [
	| modifiedTime fileReference data |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	data := 'some data'.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: data asByteArray ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime.
	self
		assert: data asByteArray
		equals: (fileReference binaryReadStreamDo: [ :aStream | aStream upToEnd ]).
	self assert: data equals: (fileReference readStreamDo: [ :aStream | aStream upToEnd ])
]

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenFileWrittenTo [
	| modifiedTime fileReference |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	modifiedTime := self lastModificationTimeOf: fileReference.
	fileReference binaryWriteStreamDo: [ :aStream | aStream nextPutAll: 'data' asByteArray ].
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
]

{ #category : #tests }
MemoryFileSystemTest >> testModifiedTimeWhenHandleTruncated [
	| modifiedTime fileReference handle |
	fileReference := (filesystem / 'file.txt') ensureCreateFile.
	handle := fileReference openWritable: true.
	modifiedTime := self lastModificationTimeOf: fileReference.
	handle truncate.
	self assert: modifiedTime notNil.
	self deny: modifiedTime equals: fileReference modificationTime
]

{ #category : #tests }
MemoryFileSystemTest >> testNotEqualWhenCreatingNewMemoryFileSystem [
	| other |
	other := FileSystem memory.
	self deny: other equals: filesystem
]

{ #category : #tests }
MemoryFileSystemTest >> testNotEqualWhenRequestingMemoryFileSystem [
	| other |
	other := self createFileSystem.
	self deny: other equals: filesystem
]
