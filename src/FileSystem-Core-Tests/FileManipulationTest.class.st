Class {
	#name : 'FileManipulationTest',
	#superclass : 'TestCase',
	#instVars : [
		'firstFolder',
		'secondFolder'
	],
	#category : 'FileSystem-Core-Tests-Base',
	#package : 'FileSystem-Core-Tests',
	#tag : 'Base'
}

{ #category : 'testing' }
FileManipulationTest class >> isAbstract [

	^ self = FileManipulationTest
]

{ #category : 'private' }
FileManipulationTest >> createFilesIn: root from: anArray [

	| dir path |
	dir := (root / anArray first) createDirectory.
	anArray allButFirstDo: [ :each |
			each isArray
				ifTrue: [ self createFilesIn: dir from: each ]
				ifFalse: [ (path := dir / each) writeStreamDo: [ :stream | stream nextPutAll: each ] ] ]
]

{ #category : 'private' }
FileManipulationTest >> createFilesIn: root from: anArray withoutPrefix: aString [
	"Helper method to create a directory hierarchy with files."

	| dir path |
	dir := (root / anArray first) createDirectory.
	anArray allButFirstDo: [ :each |
			each isArray
				ifTrue: [ self createFilesIn: dir from: each withoutPrefix: aString ]
				ifFalse: [ (path := dir / each) writeStreamDo: [ :stream | stream nextPutAll: (path pathString withoutPrefix: aString) ] ] ]
]

{ #category : 'running' }
FileManipulationTest >> lastModificationTimeOf: fileReference [
	"DateAndTime primUTCMicrosecondsClock is not precise across all OS so put in slight delay between calling modificationTime"

	^ [ fileReference modificationTime ] ensure: [ (Delay forMilliseconds: 100) wait ]
]

{ #category : 'running' }
FileManipulationTest >> root [

	^ self subclassResponsibility
]

{ #category : 'running' }
FileManipulationTest >> setUp [

	super setUp.
	firstFolder := (self root / 'Folder1') asFileReference
		               ensureCreateDirectory;
		               yourself.
	secondFolder := (self root / 'Folder2') asFileReference
		                ensureCreateDirectory;
		                yourself
]

{ #category : 'running' }
FileManipulationTest >> tearDown [

	firstFolder ensureDeleteAll.
	secondFolder ensureDeleteAll.
	super tearDown
]

{ #category : 'tests' }
FileManipulationTest >> testCopyFromto [

	| fileReference buffer |
	fileReference := firstFolder / 'test.file'.
	buffer := 'test data'.
	firstFolder fileSystem copyFrom: buffer readStream to: fileReference path.
	self assert: fileReference contents equals: buffer
]

{ #category : 'tests' }
FileManipulationTest >> testDirectoryMoveTo [
	"Test the scenario where the source is a directory, the destination is an existing directory and they are on separate file systems."

	| src dest count f33 |
	self createFilesIn: firstFolder from: #( src f11 f12 #( d11 f21 f22 #( d21 f31 f32 ) #( d22 f33 f34 ) ) ).

	src := firstFolder / 'src'.
	count := src allChildren size.
	dest := secondFolder / 'dest'.
	dest ensureCreateDirectory.
	f33 := dest / 'src/d11/d22/f33'.
	src copy moveTo: dest.
	self deny: src exists.
	self assert: (dest / 'src') allChildren size equals: count.
	self assert: f33 exists.
	self assert: f33 contents equals: 'f33'
]

{ #category : 'tests' }
FileManipulationTest >> testDirectoryMoveToRename [
	"Test the scenario where the source is a directory, the destination doesn't exist (its parent does) and they are on separate file systems, i.e. move and rename the source to the destination."

	| src dest count f33 |
	self createFilesIn: firstFolder from: #( src f11 f12 #( d11 f21 f22 #( d21 f31 f32 ) #( d22 f33 f34 ) ) ).

	src := firstFolder / 'src'.
	count := src allChildren size.
	dest := (secondFolder / 'dest') ensureCreateDirectory / 'renamed'.
	f33 := dest / 'd11/d22/f33'.
	src copy moveTo: dest.
	self deny: src exists.
	self assert: dest allChildren size equals: count.
	self assert: f33 exists.
	self assert: f33 contents equals: 'f33'
]

{ #category : 'tests' }
FileManipulationTest >> testEnsureCreateFileDoesNotOverwriteExistingContent [

	| file |
	file := firstFolder / 'a.txt'.
	file writeStreamDo: [ :s | s nextPutAll: 'ok' ].
	self assert: file contents equals: 'ok'.

	file ensureCreateFile.

	self assert: file contents equals: 'ok'
]

{ #category : 'tests' }
FileManipulationTest >> testModifiedTimeWhenFileCreated [

	self assert: (firstFolder / 'file.txt') ensureCreateFile modificationTime isNotNil
]

{ #category : 'tests' }
FileManipulationTest >> testStreamWriteAndRead [
	"Test writing custom collection and reading it [partially] back."

	| writer reader testData |
	testData := ByteArray new: 50 streamContents: [ :out | 1 to: 50 do: [ :i | out nextPut: i ] ].
	writer := (firstFolder / 'test.out') binaryWriteStream.
	writer next: 20 putAll: testData startingAt: 10.
	writer
		position: 10;
		nextPutAll: #[ 1 2 3 4 5 ].
	self assert: writer position equals: 15.
	writer close.

	reader := (firstFolder / 'test.out') binaryReadStream.
	self assert: reader size equals: 20.
	self assert: (reader next: 20) equals: #[ 10 11 12 13 14 15 16 17 18 19 1 2 3 4 5 25 26 27 28 29 ].

	^ self
]
