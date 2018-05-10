"
I am an abstract file system test. 
I directly test 
- the public interface of a FileSystem using these methods directly
- the FileSystem in general through the operation methods of the FileReference
"
Class {
	#name : #FileSystemTest,
	#superclass : #TestCase,
	#instVars : [
		'filesystem',
		'toDelete'
	],
	#category : #'FileSystem-Tests-Core-Base'
}

{ #category : #accessing }
FileSystemTest class >> defaultTimeLimit [
	^10 seconds
]

{ #category : #testing }
FileSystemTest class >> isAbstract [
	^ self name = #FileSystemTest
]

{ #category : #accessing }
FileSystemTest class >> packageNamesUnderTest [
	^ #('FileSystem')
]

{ #category : #testing }
FileSystemTest class >> shouldInheritSelectors [
	^ true
	
]

{ #category : #'initialize-release' }
FileSystemTest >> createFileSystem [
	self subclassResponsibility 
]

{ #category : #'initialize-release' }
FileSystemTest >> markForCleanup: anObject [
	toDelete add: (filesystem resolve: anObject)
]

{ #category : #running }
FileSystemTest >> setUp [
	super setUp.
	filesystem := self createFileSystem.
	toDelete := OrderedCollection new.
]

{ #category : #running }
FileSystemTest >> tearDown [
	toDelete
		select: [ :path | filesystem exists: path ]
		thenDo: [ :path | filesystem delete: path ].
	super tearDown	
]

{ #category : #'tests-streams-compatibility' }
FileSystemTest >> testBinaryReadStream [
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ reference binaryReadStream ] raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference binaryReadStream.
	self assert: stream contents asString equals: 'griffle'.
	stream close
]

{ #category : #'tests-streams-compatibility' }
FileSystemTest >> testBinaryReadStreamDo [
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self 
		should: [ reference binaryReadStreamDo: [ :stream | self assert: false ] ] 
		raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference readStreamDo: [ :stream | stream contents asString ]) 
		= 'griffle'
]

{ #category : #'tests-streams-compatibility' }
FileSystemTest >> testBinaryReadStreamDoIfAbsent [
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		binaryReadStreamDo: [ :stream | false ]
		ifAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference 
		binaryReadStreamDo: [ :stream | stream contents asString = 'griffle' ]
		ifAbsent: [ false ])
]

{ #category : #'tests-streams-compatibility' }
FileSystemTest >> testBinaryReadStreamIfAbsent [
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference binaryReadStreamIfAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference binaryReadStreamIfAbsent: [ false ].
	self assert: stream contents asString = 'griffle'.
	stream close
]

{ #category : #tests }
FileSystemTest >> testChildrenAt [
	| directory entries |
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self markForCleanup: directory / 'griffle'.
	self markForCleanup: directory / 'bint'.
	self markForCleanup: directory.
	
	entries := filesystem childrenAt: directory.
	
	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: Path).
		self assert: ea parent = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea basename) ]
]

{ #category : #tests }
FileSystemTest >> testChildrenSorting [
	| directory sorted |
	
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'alfa'.
	filesystem createDirectory: directory / 'beta'.
	
	self markForCleanup: directory / 'alfa'.
	self markForCleanup: directory / 'beta'.
	self markForCleanup: directory.
	
	sorted := (filesystem childrenAt: directory) sorted.
	self assert: sorted size = 2.
	self assert: (sorted at: 1) basename = 'alfa'.
	self assert: (sorted at: 2) basename = 'beta'.
]

{ #category : #tests }
FileSystemTest >> testChildrenSortingRoot [
	| directory1 directory2 |
	"self skip."
	
	directory1 := Path * 'plonk1'.
	directory2 := Path * 'plonk2'.
	
	filesystem createDirectory: directory1.
	filesystem createDirectory: directory2.
	
	self markForCleanup: directory1.
	self markForCleanup: directory2.
	
	self assert: filesystem workingDirectory children sorted size = filesystem workingDirectory children size
]

{ #category : #tests }
FileSystemTest >> testCopy [
	| out in contents |
	
	self
		markForCleanup: 'gooly';
		markForCleanup: 'plonk'.
		
	out := (filesystem workingDirectory / 'gooly') writeStream.
	[ out nextPutAll: 'gooly' ] ensure: [ out close ].
	filesystem copy: 'gooly' to: 'plonk'.
	
	in := (filesystem workingDirectory / 'plonk') readStream.
	contents := [ in contents asString ] ensure: [ in close ].
	self assert: contents equals: 'gooly'
]

{ #category : #tests }
FileSystemTest >> testCopyAndDelete [
	"Check that FileSystem>>copyAndDelete:to: works within a filesystem.
	DiskFileSystemTest>>testMoveMemoryToDisk checks that #copyAndDelete:to: works across filesystems."
	| folder testString f1 f1s f2 |
	
	folder := filesystem workingDirectory.
	testString := 'To be copied and deleted'.

	f1 := folder / 'f1'.
	f1s := f1 writeStream.
	[ f1s nextPutAll: testString ] ensure: [ f1s close ].
	f2 := folder / 'f2'.
	
	"Cleanup after running"
	self 
		markForCleanup: f1;
		markForCleanup: f2.	
	
	filesystem copyAndDelete: f1 to: f2.
	self deny: f1 exists.
	self assert: f2 exists.
	self assert: f2 contents equals: testString.
]

{ #category : #tests }
FileSystemTest >> testCopyDestExists [
	| out |
	
	self 
		markForCleanup: 'gooly'; 
		markForCleanup: 'plonk'.
		
	out := (filesystem  workingDirectory / 'gooly') writeStream.
	[out nextPutAll: 'gooly'] ensure: [out close].

	(filesystem  workingDirectory / 'plonk') writeStream close.
	
	self 
		should: [filesystem copy: 'gooly' to: 'plonk']
		raise: FileExists
]

{ #category : #tests }
FileSystemTest >> testCopySourceDoesntExist [
	self 
		should: [filesystem copy: 'plonk' to: 'griffle']
		raise: FileDoesNotExistException
]

{ #category : #tests }
FileSystemTest >> testCopyWithCorrectBasename [
        | directory |
        self
                markForCleanup: 'gooly';
                markForCleanup: 'plonk'.
        directory := filesystem workingDirectory.
        (directory / 'gooly') ensureCreateFile.
        directory / 'gooly' copyTo: directory / 'plonk'.
        self assert: (directory / 'plonk') exists.
        self assert: (directory childNames includes: 'plonk')
]

{ #category : #tests }
FileSystemTest >> testCreateDirectory [
	| path |
	path := filesystem workingDirectory / 'plonk' / 'griffle'.
	(filesystem workingDirectory / 'plonk') ensureCreateDirectory.
	self shouldnt: [path createDirectory] raise:Error.
	self assert: path exists.
	(filesystem workingDirectory / 'plonk' )deleteAll.
]

{ #category : #tests }
FileSystemTest >> testCreateDirectoryExists [
	| path |
	
	path := Path * 'griffle'.
	self markForCleanup: path.
	
	filesystem createDirectory: path.
	self 
		should: [filesystem createDirectory: path]
		raise: DirectoryExists.
		

	
	

]

{ #category : #tests }
FileSystemTest >> testCreateDirectoryNoParent [
	| path |
	path := Path * 'griffle' / 'nurp'.
	self 
		should: [filesystem createDirectory: path]
		raise: DirectoryDoesNotExist.
	
	

]

{ #category : #tests }
FileSystemTest >> testCreateDirectoryNotCreateParent [
	| path |
	path := filesystem workingDirectory / 'plonk' / 'griffle'.
	self should:[path createDirectory] raise: DirectoryDoesNotExist.
	self assert: path exists not.
]

{ #category : #tests }
FileSystemTest >> testCreateFile [
	| path |
	path := filesystem workingDirectory / 'plonk' / 'griffles'.
	(filesystem workingDirectory / 'plonk') ensureCreateDirectory.
	self shouldnt: [ path createFile] raise:Error.
	self assert:path exists .
	(filesystem workingDirectory / 'plonk') deleteAll.
]

{ #category : #tests }
FileSystemTest >> testCreateFileNotCreateParent [
	| path |
	path := '/plonk/griffles' asFileReference.
	self should:[path createFile] raise: DirectoryDoesNotExist .
	self assert: path exists not.
]

{ #category : #tests }
FileSystemTest >> testDefaultWorkingDirectory [
	self assert: filesystem workingDirectory isRoot
]

{ #category : #tests }
FileSystemTest >> testDelete [
	"Unlike ensureDelete, delete raises an exception if the file does not exist."
	| reference |
	
	reference := filesystem workingDirectory / 'does-not-exist'.
	self deny: reference exists.
	
	self 
		should: [ reference delete ]
		raise: FileDoesNotExist.
		
		
	reference := ( filesystem workingDirectory / 'file') ensureCreateFile.
	reference delete.
	
	self deny: reference exists. 
		

]

{ #category : #tests }
FileSystemTest >> testDelimiter [
	self assert: filesystem delimiter isCharacter
]

{ #category : #tests }
FileSystemTest >> testDirectory [
	| path |
	
	path := Path * 'plonk'.
	self markForCleanup: path.
	
	filesystem createDirectory: path.
	
	self assert: (filesystem exists: path).
	self assert: (filesystem isDirectory: path).
	
	filesystem delete: path.
	self deny: (filesystem isFile: path).
	self deny: (filesystem exists: path)
]

{ #category : #tests }
FileSystemTest >> testEnsureDirectory [
	| path |
	
	path := Path * 'plonk'.
	self markForCleanup: path.
	
	filesystem ensureCreateDirectory: path.
	self assert: (filesystem isDirectory: path).
]

{ #category : #tests }
FileSystemTest >> testEnsureDirectoryCreatesParent [
	| path |
	path := Path * 'plonk' / 'griffle'.
	self markForCleanup: path.
	self markForCleanup: path parent.
	filesystem ensureCreateDirectory: path.
	self assert: (filesystem isDirectory: Path * 'plonk').
	self assert: (filesystem isDirectory: path)
]

{ #category : #tests }
FileSystemTest >> testEnsureDirectoryExists [
	| path |
	path := Path * 'plonk'.
	self markForCleanup: path.
	filesystem createDirectory: path.
	filesystem ensureCreateDirectory: path
]

{ #category : #tests }
FileSystemTest >> testEntriesAt [
	| directory entries |
	directory := Path * 'plonk'.
	
	filesystem createDirectory: directory.
	filesystem createDirectory: directory / 'griffle'.
	filesystem createDirectory: directory / 'bint'.
	
	self 
		markForCleanup: directory / 'griffle';
		markForCleanup: directory / 'bint';
		markForCleanup: directory.
	
	entries := filesystem entriesAt: directory.
	self assert: entries size = 2.
	entries do: [ :ea | 
		self assert: (ea isKindOf: FileSystemDirectoryEntry).
		self assert: ea reference parent path = (filesystem resolve: directory).
		self assert: (#('griffle' 'bint' ) includes: ea reference basename).
		self assert: ea isDirectory ]
]

{ #category : #tests }
FileSystemTest >> testEntryAt [
	| path1 path2 entry1 entry2  |

	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	self markForCleanup: path2.
	
	filesystem createDirectory: path1.
	(Delay forSeconds: 2) wait. "#creationTime seems limited to 1 second resolution"
	filesystem createDirectory: path2.
	
	entry1 := filesystem entryAt: path1.
	entry2 := filesystem entryAt: path2.
	
	self assert: entry1 isDirectory.
	self assert: entry2 isDirectory.
	self assert: entry1 reference = (filesystem referenceTo: path1) asAbsolute.
	self assert: entry2 reference = (filesystem referenceTo: path2) asAbsolute.

	self assert: entry1 creationTime < entry2 creationTime.
	self assert: entry1 modificationTime < entry2 modificationTime.

]

{ #category : #tests }
FileSystemTest >> testFile [
	| path |
	
	path := Path * 'gooly'.
	self markForCleanup: path.
	
	(filesystem workingDirectory resolve: path) writeStream close.
	self assert: (filesystem exists: path).
	self deny: (filesystem isDirectory: path).
	self assert: (filesystem isFile: path).
	
	filesystem delete: path.
	self deny: (filesystem exists: path)
]

{ #category : #tests }
FileSystemTest >> testFileNames [
	| reference |
	#('test one' 'test with two' 'test-äöü' 'test.äöü') do: [ :each |
		reference := filesystem workingDirectory / each.
		self assert: reference basename = each.
		self deny: reference exists.
		reference
			writeStreamDo: [ :stream | stream nextPutAll: 'gooly' ]
			ifPresent: [ self fail ].
		[	self assert: reference exists.
			self assert: (filesystem workingDirectory children
				anySatisfy: [ :ref | ref = reference ]) ]
		ensure: [ reference delete ] ]
]

{ #category : #tests }
FileSystemTest >> testMoveTo [
	| base file folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	file moveTo: (folder / 'newFile').
	self deny: (base / 'file') exists.
	self assert: (folder / 'newFile') exists.
]

{ #category : #tests }
FileSystemTest >> testMoveToFailingExistingDestination [
	| base file folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	(folder / 'newFile') ensureCreateFile.
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	"Destination exists already"
	self should: [ file moveTo: (folder / 'newFile') ] raise: Error.
	self assert: (base / 'file') exists.
	self assert: (folder / 'newFile') exists.
]

{ #category : #tests }
FileSystemTest >> testMoveToFailingMissingDestination [
	| base file |
	
	base := filesystem workingDirectory.
	
	file := (base / 'file') ensureCreateFile.
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder') ;
		markForCleanup: (base / 'file').	
	
	"Destination exists already"
	self deny: (base / 'folder') exists.
	self should: [ file moveTo: (base / 'folder' / 'newFile') ] raise: Error.
	self assert: (base / 'file') exists.
	self deny: (base / 'folder' / 'newFile') exists.
]

{ #category : #tests }
FileSystemTest >> testMoveToFailingMissingSource [
	| base folder |
	
	base := filesystem workingDirectory.
	
	folder := (base / 'folder') ensureCreateDirectory. 
	
	"Cleanup after running"
	self 
		markForCleanup: (base / 'folder' / 'newFile');
		markForCleanup: (base / 'folder').
	
	self deny: (base / 'file') exists.
	"Destination exists already"
	self should: [ (base / 'file') moveTo: (folder / 'newFile') ] raise: Error.
	self deny: (base / 'file') exists.
	self deny: (folder / 'newFile') exists.
]

{ #category : #tests }
FileSystemTest >> testNonExistentEntryAt [
	| path1 path2  |
	path1 := Path * 'plonk1'.
	path2 := Path * 'plonk2'.
	self markForCleanup: path1.
	filesystem createDirectory: path1.

	self shouldnt: [ filesystem entryAt: path1 ] raise: FileDoesNotExistException.
	self should: [ filesystem entryAt: path2 ] raise: FileDoesNotExistException
]

{ #category : #tests }
FileSystemTest >> testNonExistentFileSize [
	| base file1 file2 |
	
	base := filesystem workingDirectory.
	file1 := (base / 'file1') ensureCreateFile.
	file2 := (base / 'file2').
	self markForCleanup: base / 'file1'.
	
	self shouldnt: [ file1 size ] raise: FileDoesNotExistException.
	self should: [ file2 size ] raise: FileDoesNotExistException
]

{ #category : #'tests-streams' }
FileSystemTest >> testReadStream [
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self should: [ reference readStream ] raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStream.
	self assert: stream contents asString equals: 'griffle'.
	stream close
]

{ #category : #'tests-streams' }
FileSystemTest >> testReadStreamDo [
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self 
		should: [ reference readStreamDo: [ :stream | self assert: false ] ] 
		raise: FileDoesNotExistException.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference readStreamDo: [ :stream | stream contents asString ]) 
		= 'griffle'
]

{ #category : #'tests-streams' }
FileSystemTest >> testReadStreamDoIfAbsent [
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		readStreamDo: [ :stream | false ]
		ifAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	self assert: (reference 
		readStreamDo: [ :stream | stream contents asString = 'griffle' ]
		ifAbsent: [ false ])
]

{ #category : #'tests-streams' }
FileSystemTest >> testReadStreamIfAbsent [
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference readStreamIfAbsent: [ true ]).
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStreamIfAbsent: [ false ].
	self assert: stream contents asString = 'griffle'.
	stream close
]

{ #category : #'tests-streams' }
FileSystemTest >> testReadingAfterWriting [
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self deny: reference exists.
	reference writeStreamDo: [ :ws | ws nextPutAll: 'griffle' ].
	stream := reference readStream.
	self assert: stream contents equals: 'griffle'.
	stream close
]

{ #category : #tests }
FileSystemTest >> testReferenceTo [
	|path|
	"use a relative path since absolute path behavior differs on mac, linux vs win native filesystems"
	path := Path * 'a' / 'b'.
	self assert: (filesystem referenceTo: 'a/b') path = path.
]

{ #category : #tests }
FileSystemTest >> testRoot [
	self assert: filesystem root fileSystem = filesystem.
	self assert: filesystem root path = Path root.
	self assert: filesystem root isRoot.
]

{ #category : #tests }
FileSystemTest >> testRootExists [
	self assert: (filesystem exists: Path root)
]

{ #category : #tests }
FileSystemTest >> testRootIsDirectory [
	self assert: (filesystem isDirectory: Path root)
]

{ #category : #tests }
FileSystemTest >> testRootIsNotAFile [
	self deny: (filesystem isFile: Path root)
]

{ #category : #tests }
FileSystemTest >> testWorking [
	self assert: filesystem workingDirectory fileSystem = filesystem.
	self assert: filesystem workingDirectory path = filesystem workingDirectoryPath
]

{ #category : #'tests-streams' }
FileSystemTest >> testWriteStream [
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	stream := reference writeStream.
	stream nextPutAll: 'griffle'.
	stream close.
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	stream := reference writeStream.
	stream close
]

{ #category : #'tests-streams' }
FileSystemTest >> testWriteStreamDo [
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference writeStreamDo: [ :stream |
		stream nextPutAll: 'griffle'.
		true ]).
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference writeStreamDo: [ :stream | true ])
]

{ #category : #'tests-streams' }
FileSystemTest >> testWriteStreamDoIfPresent [
	| reference |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	self assert: (reference 
		writeStreamDo: [ :stream |
			stream nextPutAll: 'griffle'.
			true ]
		ifPresent: [ false ]).
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference 
		writeStreamDo: [ :stream | true ]
		ifPresent: [ true ])
]

{ #category : #'tests-streams' }
FileSystemTest >> testWriteStreamIfPresent [
	| reference stream |
	self markForCleanup: (reference := filesystem workingDirectory / 'griffle').
	stream := reference writeStreamIfPresent: [ false ].
	stream nextPutAll: 'griffle'.
	stream close.
	self assert: (filesystem workingDirectory / 'griffle') isFile.
	self assert: (reference writeStreamIfPresent: [ true ])
]
