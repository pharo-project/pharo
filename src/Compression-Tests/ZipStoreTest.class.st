"
I test ZipStore, the file system of the contents of a ZIP archive (FileSystem zip: aReference): reading the members of an archive, and writing them back when the file system is closed.
"
Class {
	#name : 'ZipStoreTest',
	#superclass : 'TestCase',
	#instVars : [
		'fileSystem',
		'zipFile'
	],
	#category : 'Compression-Tests-Archives',
	#package : 'Compression-Tests',
	#tag : 'Archives'
}

{ #category : 'helpers' }
ZipStoreTest >> reread [

	^ ZipArchive new readFrom: zipFile
]

{ #category : 'running' }
ZipStoreTest >> setUp [

	super setUp.
	fileSystem := FileSystem memory.
	zipFile := fileSystem / 'archive.zip'
]

{ #category : 'tests' }
ZipStoreTest >> testBinaryFilesAreKept [

	| bytes |
	bytes := (0 to: 255) asByteArray.
	self writeArchive: [ :archive | archive addString: bytes as: 'data.bin' ].
	self zipDo: [ :root | root / 'other.bin' binaryWriteStreamDo: [ :stream | stream nextPutAll: bytes reversed ] ].
	self assert: (self reread contentsOf: 'data.bin') equals: bytes.
	self assert: (self reread contentsOf: 'other.bin') equals: bytes reversed
]

{ #category : 'tests' }
ZipStoreTest >> testCreateFile [

	self zipDo: [ :root |
			(root / 'dir') ensureCreateDirectory.
			root / 'dir' / 'new.txt' writeStreamDo: [ :stream | stream nextPutAll: 'created' ] ].
	self assert: (self reread contentsOf: 'dir/new.txt') asString equals: 'created'.
	self zipDo: [ :root | self assert: (root / 'dir' / 'new.txt') contents equals: 'created' ]
]

{ #category : 'tests' }
ZipStoreTest >> testMemberNamesArePaths [

	self zipDo: [ :root |
			(root / 'dir' / 'sub') ensureCreateDirectory.
			root / 'dir' / 'sub' / 'a.txt' writeStreamDo: [ :stream | stream nextPutAll: 'a' ].
			(root / 'empty') ensureCreateDirectory ].
	self assert: self reread memberNames asArray sorted equals: #( 'dir/' 'dir/sub/' 'dir/sub/a.txt' 'empty/' )
]

{ #category : 'tests' }
ZipStoreTest >> testModifyFile [

	self writeArchive: [ :archive | archive addDeflateString: 'original contents' as: 'a.txt' ].
	self zipDo: [ :root |
			(root / 'a.txt') ensureDelete.
			root / 'a.txt' writeStreamDo: [ :stream | stream nextPutAll: 'new' ].
			self assert: (root / 'a.txt') contents equals: 'new' ].
	self assert: (self reread contentsOf: 'a.txt') asString equals: 'new'
]

{ #category : 'tests' }
ZipStoreTest >> testReadMembers [

	self writeArchive: [ :archive | archive addDeflateString: 'hello' as: 'dir/a.txt' ].
	self zipDo: [ :root |
			self assert: (root / 'dir' / 'a.txt') contents equals: 'hello'.
			self assert: (root / 'dir' / 'a.txt') size equals: 5 ]
]

{ #category : 'helpers' }
ZipStoreTest >> writeArchive: aBlock [

	| archive |
	archive := ZipArchive new.
	aBlock value: archive.
	archive writeToFile: zipFile.
	archive close
]

{ #category : 'helpers' }
ZipStoreTest >> zipDo: aBlock [
	"Open the archive as a file system, evaluate aBlock with its root, and write the archive back"

	| zip |
	zip := (FileSystem zip: zipFile) open.
	[ aBlock value: zip root ] ensure: [ zip close ]
]
