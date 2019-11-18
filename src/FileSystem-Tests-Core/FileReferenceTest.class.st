"
SUnit tests for file reference
"
Class {
	#name : #FileReferenceTest,
	#superclass : #TestCase,
	#type : #variable,
	#instVars : [
		'filesystem'
	],
	#category : #'FileSystem-Tests-Core-Base'
}

{ #category : #support }
FileReferenceTest >> createFile: aPath [
	filesystem ensureCreateDirectory: aPath parent.
	(filesystem resolve: aPath) writeStream close
]

{ #category : #running }
FileReferenceTest >> setUp [
	super setUp.
	filesystem := FileSystem memory
]

{ #category : #tests }
FileReferenceTest >> testAllChildren [
	"allChildren returns all the files and folders recursively nested in a reference"
	| ref children |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	children := ref allChildren.
	"all children returns the nodes: '/alpha', '/alpha/beta',  '/alpha/beta/delta', and '/alpha/gamma'."
	self assert: children size equals: 4.
	children
		do: [ :child | 
			self assert: child class equals: FileReference.
			self assert: (ref = child or: [ ref contains: child ]) ].
	self assert: (children collect: [ :ea | ea basename ]) equals: #('alpha' 'beta' 'gamma' 'delta') ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testAllDirectories [
	"allDirectories returns all folders recursively nested in a reference"
	| ref children |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	children := ref allDirectories.
	"all children returns the directories: '/alpha', '/alpha/beta', and '/alpha/gamma'."
	self assert: children size equals: 4.
	children
		do: [ :child | 
			self assert: child class equals: FileReference.
			self assert: (ref = child or: [ ref contains: child ]) ].
	self assert: (children collect: [ :ea | ea basename ]) equals: #('alpha' 'beta' 'gamma' 'delta') ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testAllEntries [
	| ref entries |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	entries := ref allEntries.
	self assert: entries size equals: 4.
	entries do: [:entry |
		self assert: entry class equals: MemoryDirectoryEntry.
		self assert: (ref = entry reference or: [ref contains: entry reference])].
	self assert: (entries collect: [ :ea | ea basename ]) equals: #('alpha' 'beta' 'gamma' 'delta') ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testAsAbsoluteConverted [
	"Converts a relative reference to absolute, and asserts
	that it's absolute and still has the same path."

	| ref absolute |
	ref := filesystem * 'plonk'.
	absolute := ref asAbsolute.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) equals: 'plonk'
]

{ #category : #tests }
FileReferenceTest >> testAsAbsoluteIdentity [
	| ref |
	ref := filesystem / 'plonk'.
	self assert: ref asAbsolute identicalTo: ref
]

{ #category : #tests }
FileReferenceTest >> testAsReference [
	| ref |
	ref := filesystem * 'plonk'.
	self assert: ref asFileReference identicalTo: ref
]

{ #category : #tests }
FileReferenceTest >> testBaseAndExtension [
	| noExtension simpleExtension complexExtension |
	noExtension := filesystem * 'plonk'.
	self assert: noExtension extension equals: ''.

	"We create a reference to the plonk/griffle.taz in the context of filesystem"
	simpleExtension := filesystem * 'plonk' / 'griffle.taz'.
	self assert: simpleExtension base equals: 'griffle'.
	self assert: simpleExtension extension equals: 'taz'.

	"Note that the extension of a complex extension starts after the last extension delimiter"
	complexExtension := filesystem * 'plonk' / 'griffle.taz.txt'.
	self assert: complexExtension base equals: 'griffle.taz'.
	self assert: complexExtension extension equals: 'txt'
]

{ #category : #tests }
FileReferenceTest >> testBasename [
	| ref |
	ref := filesystem root.
	self assert: ref basename equals: '/'.
	ref := filesystem * 'plonk' / 'griffle'.
	self assert: ref basename equals: 'griffle'.
	ref := filesystem * 'plonk' / 'griffle.taz'.
	self assert: ref basename equals: 'griffle.taz'
]

{ #category : #tests }
FileReferenceTest >> testBasenameWithoutExtension [
	
	| ref |	
	ref := filesystem root.
	self assert: ref basename equals: '/'.
	
	ref := filesystem * 'plonk' / 'griffle'.
	self assert: ref basenameWithoutExtension equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle'.
	
	ref := filesystem * 'plonk' / 'griffle.taz'.
	self assert: ref basenameWithoutExtension equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle.taz') equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	
	ref := filesystem * 'plonk' / 'griffle.taz.zork'.
	self assert: ref basenameWithoutExtension equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'zork') equals: 'griffle.taz'.
	self assert: (ref basenameWithoutExtension: 'taz.zork') equals: 'griffle'.
	self assert: (ref basenameWithoutExtension: 'griffle.taz.zork') equals: 'griffle.taz.zork'.
	self assert: (ref basenameWithoutExtension: 'taz') equals: 'griffle.taz.zork'.
]

{ #category : #tests }
FileReferenceTest >> testCanonicalization [

	| ref |

	ref := 'a/b/c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b' 'c').

	ref := '/a/b/c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b' 'c').

	ref := '../a/b/c' asFileReference canonicalize.
	self assert: ref path segments equals: #('..' 'a' 'b' 'c').

	ref := 'a/b/c/..' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b').

	ref := '/a/b/c/..' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'b').

	ref := 'a/b/../c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'c').

	ref := '/a/b/../c' asFileReference canonicalize.
	self assert: ref path segments equals: #('a' 'c').

]

{ #category : #tests }
FileReferenceTest >> testChildDirectories [
	| childDirectories |
	[ filesystem createDirectory: '/beta'.
	filesystem createDirectory: '/gamma'.
	filesystem / 'delta' writeStreamDo: [ :stream | stream nextPutAll: '1' ].
	filesystem / 'epsilon' writeStreamDo: [ :stream | stream nextPutAll: '2' ].
	childDirectories := filesystem root directories.
	self assert: childDirectories size equals: 2.
	childDirectories
		do: [ :each | 
			self assert: each class equals: FileReference.
			self
				assert: each isDirectory
				description: 'Collection should not contain references to files.' ] ]
		ensure: [ (filesystem / 'beta') ensureDeleteAll.
			(filesystem / 'gamma') ensureDeleteAll.
			(filesystem / 'delta') ensureDelete.
			(filesystem / 'epsilon') ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testChildFiles [
	| childFiles |
	[ filesystem createDirectory: '/beta'.
	filesystem createDirectory: '/gamma'.
	filesystem / 'delta' writeStreamDo: [ :stream | stream nextPutAll: '1' ].
	filesystem / 'epsilon' writeStreamDo: [ :stream | stream nextPutAll: '2' ].
	childFiles := filesystem root files.
	self assert: childFiles size equals: 2.
	childFiles
		do: [ :each | 
			self assert: each class equals: FileReference.
			self assert: each isFile description: 'Collection should not contain references to directories.' ] ]
		ensure: [ (filesystem / 'beta') ensureDeleteAll.
			(filesystem / 'gamma') ensureDeleteAll.
			(filesystem / 'delta') ensureDelete.
			(filesystem / 'epsilon') ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testChildOfPath [
	| parent  child |
	parent := Path / 'griffle'.
	child := filesystem / 'griffle' / 'nurb'.
	self deny: (child isChildOf: parent).
	self deny: (parent isChildOf: child)
]

{ #category : #tests }
FileReferenceTest >> testChildOfReference [
	| parent  child |
	parent := filesystem / 'griffle'.
	child := filesystem / 'griffle' / 'nurb'.
	self assert: (child isChildOf: parent).
	self deny: (parent isChildOf: child)
]

{ #category : #tests }
FileReferenceTest >> testChildren [
	| ref children |
	[ filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	children := ref children.
	self assert: children size equals: 2.
	children
		do: [ :child | 
			self assert: child class equals: FileReference.
			self assert: (child isChildOf: ref).
			self assert: (#('beta' 'gamma') includes: child basename) ] ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testCommaAddsExtension [
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref , 'griffle'.
	self assert: result basename equals: 'plonk.griffle'
]

{ #category : #tests }
FileReferenceTest >> testCommaAddsExtensionAgain [
	| ref result |
	ref := filesystem * 'plonk.griffle'.
	result := ref , 'nurp'.
	self assert: result basename equals: 'plonk.griffle.nurp'
]

{ #category : #tests }
FileReferenceTest >> testContainsLocator [
	| ref |
	ref := FileLocator imageDirectory resolve parent.
	self assert: (ref contains: FileLocator image)
]

{ #category : #tests }
FileReferenceTest >> testContainsPath [
	| ref |
	ref := filesystem  * 'griffle'.
	self assert: (ref contains: (ref / 'nurp') path)
]

{ #category : #tests }
FileReferenceTest >> testContainsReference [
	| ref |
	ref := filesystem * 'griffle'.
	self assert: (ref contains: ref / 'nurp')
]

{ #category : #tests }
FileReferenceTest >> testContents [
	| ref contents |
	contents := '12345 abcdf!'.
	ref := filesystem * 'file'.
	[ ref writeStreamDo: [ :stream | stream nextPutAll: contents ].
	self assert: ref contents asString equals: contents ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testDeleteAll [
	"allChildren returns all the files and folders recursively nested in a reference"
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	ref deleteAll.
	self deny: ref exists.
	self deny: (ref / 'beta') exists.
	self deny: (ref / 'beta' / 'delta') exists.
	self deny: (ref / 'beta' / 'gamma') exists ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testDeleteAllChildren [
	"allChildren returns all the files and folders recursively nested in a reference"
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	ref deleteAllChildren.
	self assert: ref exists.
	self deny: (ref / 'beta') exists.
	self deny: (ref / 'beta' / 'delta') exists.
	self deny: (ref / 'beta' / 'gamma') exists ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testDeleteIfAbsent [
	| flag reference |
	flag := false.
	reference := filesystem / 'plonk'.
	reference ensureCreateFile.
	[ reference exists
		ifFalse: [ self error ].
	reference deleteIfAbsent: [ flag := true ].
	self deny: flag.
	reference exists
		ifTrue: [ self error ].
	reference deleteIfAbsent: [ flag := true ].
	self assert: flag ]
		ensure: [ reference ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testDoesContainReferenceFileSystem [
	| ref other |
	ref := filesystem * 'griffle'.
	other := filesystem / 'griffle' / 'nurp'.
	self assert: (ref contains: other)
]

{ #category : #tests }
FileReferenceTest >> testDoesNotContainReferenceWhenUsingDifferentInstancesOfMemoryFileSystem [
	| ref other |
	ref := filesystem * 'griffle'.
	other := FileSystem memory / 'griffle' / 'nurp'.
	self deny: (ref contains: other)
]

{ #category : #tests }
FileReferenceTest >> testDoesntContainLocator [
	| ref |
	ref := filesystem * 'griffle'. 
	self deny: (ref contains: FileLocator image)
]

{ #category : #tests }
FileReferenceTest >> testDoesntContainPath [
	| ref |
	ref := filesystem * 'griffle'.
	self deny: (ref contains: (Path * 'nurp'))
]

{ #category : #tests }
FileReferenceTest >> testDoesntContainReferencePath [
	| ref other |
	ref := filesystem * 'griffle'.
	other := filesystem * 'nurp'.
	self deny: (ref contains: other)
]

{ #category : #tests }
FileReferenceTest >> testEnsureDelete [
	| reference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateFile.
	[ self assert: reference exists.
	reference ensureDelete.
	self deny: reference exists	"No-op if file does not exist" ]
		ensure: [ reference ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testEnsureDeleteAll [
	| reference childReference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateDirectory.
	[ self assert: reference exists.
	childReference := reference / 'child'.
	childReference ensureCreateFile.
	self assert: childReference exists.
	reference ensureDeleteAll.
	self deny: childReference exists.
	self deny: reference exists ]
		ensure: [ reference ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testEnsureDeleteNonEmptyDirectory [
	| reference childReference |
	reference := filesystem / 'plonk'.	"Deletes the file if it exists"
	reference ensureCreateDirectory.
	[ self assert: reference exists.
	childReference := reference / 'child'.
	childReference ensureCreateFile.
	self assert: childReference exists.
	self should: [ reference ensureDelete ] raise: DirectoryIsNotEmpty ]
		ensure: [ reference ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testEntries [
	[ | ref entries |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	entries := ref entries.
	self assert: entries size equals: 2.
	entries do: [:entry |
		self assert: entry class equals: MemoryDirectoryEntry.
		self assert: (entry reference isChildOf: ref).
		self assert: (#('beta' 'gamma') includes: entry reference basename)] ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testEqual [
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem * 'plonk'.
	self deny: a identicalTo: b.
	self assert: a equals: b
]

{ #category : #tests }
FileReferenceTest >> testEqualityRelativeVsAbsolute [

	| f1 f2 |

	f1 := FileLocator workingDirectory / 'pharo-local'.
	f2 := f1 asAbsolute.
	self assert: f1 equals: f2
]

{ #category : #tests }
FileReferenceTest >> testExists [
	| reference |
	reference := filesystem / 'plonk'.
	reference ensureCreateFile.
	[ self assert: reference exists.
	reference delete.
	self deny: reference exists ]
		ensure: [ reference ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testGlob [
	[ | ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem root.
	children := ref glob: [ :node | true ].
	self assert: children size identicalTo: 4.	"including root"
	children := ref glob: [ :node | node basename size > 1 ].
	self assert: children size identicalTo: 3.	"without root"
	children := ref glob: [ :node | node basename = #gamma ].
	self assert: children size identicalTo: 1.	"gamma"
	self assert: children first basename equals: #gamma ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testGrandchildOfReference [
	| griffle  nurb |
	griffle := filesystem / 'griffle'.
	nurb := filesystem / 'griffle' / 'plonk' / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle)
]

{ #category : #tests }
FileReferenceTest >> testHasChildren [
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	self assert: ref hasChildren.
	self assert: (ref / 'beta') hasChildren.
	self deny: (ref / 'beta' / 'delta') hasChildren.
	self deny: (ref / 'beta' / 'gamma') hasChildren ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testHasDirectories [
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	(filesystem / 'alpha' / 'beta' / 'delta') ensureCreateFile.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	self assert: ref hasDirectories.
	self deny: (ref / 'beta') hasDirectories.
	self deny: (ref / 'beta' / 'gamma') hasDirectories ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testHasFiles [
	[ | ref |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	(filesystem / 'alpha' / 'beta' / 'delta') ensureCreateFile.
	filesystem createDirectory: '/alpha/beta/eta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	self deny: ref hasFiles.
	self assert: (ref / 'beta') hasFiles.
	self deny: (ref / 'beta' / 'gamma') hasFiles ]
		ensure: [ (filesystem / 'alpha') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testIndicator [
	| ref |
	ref := filesystem * 'plonk' / 'griffle'.
	self deny: ref exists.
	self assert: ref indicator equals: '?'.
	ref := filesystem workingDirectory / 'plonk'.
	self deny: ref exists.
	ref createDirectory.
	[ self assert: ref exists.
	self assert: ref isDirectory.
	self assert: ref indicator equals: '/' ]
		ensure: [ ref delete ].
	ref := filesystem workingDirectory / 'plonk'.
	self deny: ref exists.
	[ ref writeStreamDo: [ :stream | stream nextPutAll: 'foo' ] ifPresent: [ self fail ].
	self assert: ref exists.
	self assert: ref isFile.
	self assert: ref indicator equals: '' ]
		ensure: [ ref delete ]
]

{ #category : #tests }
FileReferenceTest >> testIsAbsolute [
	self assert: (filesystem / 'plonk') isAbsolute
]

{ #category : #tests }
FileReferenceTest >> testIsNotAbsolute [
	self deny: (filesystem * 'plonk') isAbsolute
]

{ #category : #tests }
FileReferenceTest >> testIsNotRelative [
	self deny: (filesystem / 'plonk') isRelative
]

{ #category : #tests }
FileReferenceTest >> testIsNotRoot [
	self deny: (filesystem / 'plonk') isRoot
]

{ #category : #tests }
FileReferenceTest >> testIsRelative [
	self assert: (filesystem * 'plonk') isRelative
]

{ #category : #tests }
FileReferenceTest >> testIsRoot [
	self assert: (filesystem root) isRoot
]

{ #category : #'tests - handy utils' }
FileReferenceTest >> testLastFilesForExtension [
	"This test verifies that we have the following ordering: 3 4 5 33 49 51 101"
	| childFiles |
	[ filesystem createDirectory: 'dir'.
	filesystem / 'dir' / 'games.51.ston' writeStreamDo: [ :stream | stream nextPutAll: '51' ].
	filesystem / 'dir' / 'games.3.ston' writeStreamDo: [ :stream | stream nextPutAll: '3' ].
	filesystem / 'dir' / 'games.33.ston' writeStreamDo: [ :stream | stream nextPutAll: '33' ].
	filesystem / 'dir' / 'games.4.ston' writeStreamDo: [ :stream | stream nextPutAll: '4' ].
	filesystem / 'dir' / 'games.101.ston' writeStreamDo: [ :stream | stream nextPutAll: '101' ].
	filesystem / 'dir' / 'games.49.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	filesystem / 'dir' / 'games.5.ston' writeStreamDo: [ :stream | stream nextPutAll: '5' ].
	
	childFiles := (filesystem root / 'dir') files.
	self assert: childFiles size equals: 7.
	self assert: ((filesystem root / 'dir') lastFileFor: 'games' extension: 'ston') basename equals: 'games.101.ston'.

	
			]
			ensure: [ (filesystem / 'dir') ensureDeleteAll ]
]

{ #category : #'tests - handy utils' }
FileReferenceTest >> testLastFilesForExtensionBrokenPattern [
	"when a file does not follow the pattern name.number.extension, it should be filtered out."
	[ filesystem createDirectory: 'dir'.
	filesystem / 'dir' / 'games.51.ston' writeStreamDo: [ :stream | stream nextPutAll: '51' ].
	filesystem / 'dir' / 'games.ston' writeStreamDo: [ :stream | stream nextPutAll: 'nonumber' ].
	filesystem / 'dir' / 'games.stf' writeStreamDo: [ :stream | stream nextPutAll: 'wrong extension' ].
	filesystem / 'dir' / 'games.101.ston' writeStreamDo: [ :stream | stream nextPutAll: '101' ].
	filesystem / 'dir' / 'games.250.49.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	filesystem / 'dir' / 'games.1000.1000.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	filesystem / 'dir' / 'zorglub.500.ston' writeStreamDo: [ :stream | stream nextPutAll: '5' ].

	self assert: ((filesystem root / 'dir') lastFileFor: 'games' extension: 'ston') basename equals: 'games.101.ston'.
	] ensure: [ (filesystem / 'dir') ensureDeleteAll ]
]

{ #category : #'tests - handy utils' }
FileReferenceTest >> testLastFilesForExtensionNegativeTesting [
	"When there is no file with the given pattern, then an error should be raised."
	
	[ filesystem createDirectory: 'dir'.
	filesystem / 'dir' / 'games.51.ston' writeStreamDo: [ :stream | stream nextPutAll: '51' ].
	filesystem / 'dir' / 'games.4.ston' writeStreamDo: [ :stream | stream nextPutAll: '4' ].
	filesystem / 'dir' / 'games.101.ston' writeStreamDo: [ :stream | stream nextPutAll: '101' ].
	filesystem / 'dir' / 'games.49.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	filesystem / 'dir' / 'games.5.ston' writeStreamDo: [ :stream | stream nextPutAll: '5' ].
	
	self should: [((filesystem root / 'dir') lastFileFor: 'games' extension: 'stf')] raise: Error.
   self should: [((filesystem root / 'dir') lastFileFor: 'plouf' extension: 'ston')] raise: Error.
	] ensure: [ (filesystem / 'dir') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testMakeRelative [

	| parent child relative |
	parent := filesystem / 'griffle'.
	child := filesystem / 'griffle' / 'plonk' / 'nurb'.
	relative := parent makeRelative: child.
	self assert: relative equals: (Path * 'plonk' / 'nurb')
]

{ #category : #'tests - handy utils' }
FileReferenceTest >> testNextNameForExtension [
	
	| childFiles dir |

	[ filesystem createDirectory: 'dir'.
	dir := filesystem / 'dir'.
	dir / 'games.51.ston' writeStreamDo: [ :stream | stream nextPutAll: '51' ].
	dir / 'games.3.ston' writeStreamDo: [ :stream | stream nextPutAll: '3' ].
	dir / 'games.33.ston' writeStreamDo: [ :stream | stream nextPutAll: '33' ].
	dir / 'games.4.ston' writeStreamDo: [ :stream | stream nextPutAll: '4' ].
	dir / 'games.101.ston' writeStreamDo: [ :stream | stream nextPutAll: '101' ].
	dir / 'games.49.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	dir / 'games.5.ston' writeStreamDo: [ :stream | stream nextPutAll: '5' ].
	
	childFiles := dir files.
	self assert: childFiles size equals: 7.
	self assert: (dir nextNameFor: 'games' extension: 'ston') equals: 'games.102.ston' ]
		ensure: [ (filesystem / 'dir') ensureDeleteAll ].


	[ filesystem createDirectory: 'dir'.
	dir := filesystem / 'dir'.
	dir / 'foo.bar.51.ston' writeStreamDo: [ :stream | stream nextPutAll: '51' ].
	dir / 'foo.bar.3.ston' writeStreamDo: [ :stream | stream nextPutAll: '3' ].
	dir / 'foo.bar.33.ston' writeStreamDo: [ :stream | stream nextPutAll: '33' ].
	dir / 'foo.bar.4.ston' writeStreamDo: [ :stream | stream nextPutAll: '4' ].
	dir / 'foo.bar.101.ston' writeStreamDo: [ :stream | stream nextPutAll: '101' ].
	dir / 'foo.bar.49.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	dir / 'foo.bar.5.ston' writeStreamDo: [ :stream | stream nextPutAll: '5' ].
	
	childFiles := dir files.
	self assert: childFiles size equals: 7.
	self assert: (dir nextNameFor: 'foo.bar' extension: 'ston') equals: 'foo.bar.102.ston' ]
		ensure: [ (filesystem / 'dir') ensureDeleteAll ].

]

{ #category : #'tests - handy utils' }
FileReferenceTest >> testNextNameForExtensionBrokenPattern [
	
	| childFiles |
	[ filesystem createDirectory: 'dir'.
filesystem / 'dir' / 'games.51.ston' writeStreamDo: [ :stream | stream nextPutAll: '51' ].
	filesystem / 'dir' / 'games.ston' writeStreamDo: [ :stream | stream nextPutAll: 'nonumber' ].
	filesystem / 'dir' / 'games.stf' writeStreamDo: [ :stream | stream nextPutAll: 'wrong extension' ].
	filesystem / 'dir' / 'games.101.ston' writeStreamDo: [ :stream | stream nextPutAll: '101' ].
	filesystem / 'dir' / 'games.250.49.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	filesystem / 'dir' / 'games.1000.1000.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	filesystem / 'dir' / 'zorglub.500.ston' writeStreamDo: [ :stream | stream nextPutAll: '5' ].
	
	childFiles := (filesystem root / 'dir') files.
	self assert: childFiles size equals: 7.
	self assert: ((filesystem root / 'dir') nextNameFor: 'games' extension: 'ston') equals: 'games.102.ston'.

	
			]
			ensure: [ (filesystem / 'dir') ensureDeleteAll ]
]

{ #category : #'tests - handy utils' }
FileReferenceTest >> testNextNameForExtensionNegativeTesting [
	"When there is no file with the given pattern, then we default patterned name should be returned."
	
	[ filesystem createDirectory: 'dir'.
	filesystem / 'dir' / 'games.51.ston' writeStreamDo: [ :stream | stream nextPutAll: '51' ].
	filesystem / 'dir' / 'games.4.ston' writeStreamDo: [ :stream | stream nextPutAll: '4' ].
	filesystem / 'dir' / 'games.101.ston' writeStreamDo: [ :stream | stream nextPutAll: '101' ].
	filesystem / 'dir' / 'games.49.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	filesystem / 'dir' / 'games.5.ston' writeStreamDo: [ :stream | stream nextPutAll: '5' ].

	self assert: ((filesystem root / 'dir') nextNameFor: 'games' extension: 'stf') equals: 'games.1.stf'.
   self assert: ((filesystem root / 'dir') nextNameFor: 'plouf' extension: 'ston') equals:  'plouf.1.ston'.
	] ensure: [ (filesystem / 'dir') ensureDeleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testNextVersion [

	| fs file versions |

	fs := FileSystem memory.
	file := fs / 'file.name'.
	versions := Array streamContents: [ :stream |
		5 timesRepeat: [ 
			file createFile.
			stream nextPut: file basename.
			file := file nextVersion. ] ].
	self assert: versions equals: #('file.name' 'file.1.name' 'file.2.name' 'file.3.name' 'file.4.name').

	fs := FileSystem memory.
	file := fs / 'file123.name.ext'.
	versions := Array streamContents: [ :stream |
		5 timesRepeat: [ 
			file createFile.
			stream nextPut: file basename.
			file := file nextVersion. ] ].
	self assert: versions equals: #('file123.name.ext' 'file123.name.1.ext' 'file123.name.2.ext' 'file123.name.3.ext' 'file123.name.4.ext').

	fs := FileSystem memory.
	file := fs / '123.ext'.
	versions := Array streamContents: [ :stream |
		5 timesRepeat: [ 
			file createFile.
			stream nextPut: file basename.
			file := file nextVersion. ] ].
	self assert: versions equals: #('123.ext' '123.1.ext' '123.2.ext' '123.3.ext' '123.4.ext').

]

{ #category : #tests }
FileReferenceTest >> testParent [
	| ref parent |
	ref := filesystem * 'plonk' / 'griffle'.
	parent := ref parent.
	self assert: parent class equals: ref class.
	self assert: (parent path at: 1) equals: 'plonk'
]

{ #category : #tests }
FileReferenceTest >> testParentResolutionWithAbsoluteReference [
	| base relative absolute |
	base := filesystem / '/plonk' / 'pinto'.
	relative := filesystem / 'griffle' / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute fileSystem identicalTo: relative fileSystem.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) equals: 'griffle'.
	self assert: (absolute path at: 2) equals: 'zonk'
]

{ #category : #tests }
FileReferenceTest >> testParentResolutionWithPath [
	| base relative absolute |
	base := filesystem / 'plonk' / 'pinto'.
	relative := Path parent / 'griffle' / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').
	
]

{ #category : #tests }
FileReferenceTest >> testParentResolutionWithReference [
	| base relative absolute |
	base := (filesystem / 'plonk' / 'pinto').
	relative := (filesystem referenceTo: '../griffle/zonk').
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk').
	
	
]

{ #category : #tests }
FileReferenceTest >> testParentResolutionWithRemoteReference [
	| base relative absolute |
	base := filesystem / 'plonk' / 'pinto'.
	relative := filesystem referenceTo: '../griffle/zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: absolute path segments equals: #('plonk' 'pinto' '..' 'griffle' 'zonk')
]

{ #category : #tests }
FileReferenceTest >> testParentUpTo [
	| base a b c |
	[ 
		base := filesystem workingDirectory.
		(base / 'testParentUpTo') ensureCreateDirectory.
		a := (base / 'testParentUpTo' / 'A') ensureCreateDirectory.
		b := (base / 'testParentUpTo' / 'A' / 'B') ensureCreateDirectory.
		c := (base / 'testParentUpTo' / 'A' / 'B' / 'C') ensureCreateDirectory.
		self assert: b equals: (c parentUpTo: 'B').
		self assert: a equals: (c parentUpTo: 'A').
		self assert: (base / 'testParentUpTo') equals: (c parentUpTo: 'testParentUpTo').
		self assert: base equals: (c parentUpTo: 'notAParent') 
	] ensure: [ 
		(base / 'testParentUpTo') ensureDeleteAll 
	]
]

{ #category : #tests }
FileReferenceTest >> testPathRelativeTo [
	| parent childPath relative |
	parent := filesystem / 'griffle'.
	childPath := Path / 'griffle' / 'plonk' / 'nurb'.
	relative := childPath relativeTo: parent.
	self assert: relative equals: (Path * 'plonk' / 'nurb')
]

{ #category : #tests }
FileReferenceTest >> testReadStream [
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ [ stream := ref readStream ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref delete ]
]

{ #category : #tests }
FileReferenceTest >> testReadStreamDo [
	| ref path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref readStreamDo: [ :stream | self deny: stream isNil ] ]
		ensure: [ ref delete ]
]

{ #category : #'tests stream' }
FileReferenceTest >> testReadStreamDoNotFound [
	| ref |
	ref := filesystem / 'plonk'.
	self
		should: [ref readStreamDo: [:s]]
		raise: FileDoesNotExistException
]

{ #category : #tests }
FileReferenceTest >> testReadStreamDoifAbsent [
	| ref path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref readStreamDo: [ :stream | self deny: stream isNil ] ifAbsent: [ self signalFailure: 'The file exists!' ] ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testReadStreamDoifAbsentNot [
	| ref pass |
	pass := false.
	ref := filesystem * 'plonk'.
	ref 
		readStreamDo: [:stream]
		ifAbsent: [pass := true].
	self assert: pass
]

{ #category : #tests }
FileReferenceTest >> testReadStreamIfAbsent [
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ [ stream := ref readStreamIfAbsent: [ self signalFailure: 'Should not reach here.' ] ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testReadStreamNotFound [
	| ref  |
	ref := filesystem * 'plonk'.
	self
		should: [ref readStream]
		raise: FileDoesNotExistException
]

{ #category : #tests }
FileReferenceTest >> testRelativeToPath [
	| parentPath child relative |
	parentPath := Path / 'griffle'.
	child := filesystem / 'griffle' / 'plonk' / 'nurb'.
	relative := child relativeTo: parentPath.
	self assert: relative equals: (Path * 'plonk' / 'nurb')
]

{ #category : #tests }
FileReferenceTest >> testRelativeToReference [
	| parent child relative |
	parent := filesystem / 'griffle'.
	child := filesystem  / 'griffle' / 'plonk' / 'nurb'.
	relative := child relativeTo: parent.
	self assert: relative equals: (Path * 'plonk' / 'nurb')
]

{ #category : #tests }
FileReferenceTest >> testRename [
	| file tmp originalFullName |
	[ file := (FileLocator imageDirectory / 'oldName') ensureCreateFile.
	originalFullName := file fullName.
	tmp := (FileLocator imageDirectory / 'tmp') ensureCreateDirectory.
	file renameTo: 'newName'.
	self deny: originalFullName asFileReference exists.
	self assert: file basename equals: 'newName'.
	self assert: (originalFullName asFileReference parent / 'newName') exists ]
		ensure: [ file delete.
			tmp deleteAll ]
]

{ #category : #tests }
FileReferenceTest >> testRenameTargetExists [

	| existingFile fileToRename |
	[
		existingFile := 'existingFile' asFileReference ensureCreateFile.
		fileToRename := 'fileToRename' asFileReference ensureCreateFile.
		self should: [ fileToRename renameTo: existingFile basename ] raise: FileExists ]
		ensure: [
			existingFile delete.
			fileToRename delete ].
]

{ #category : #tests }
FileReferenceTest >> testResolve [
	| ref |
	ref := filesystem / 'griffle'.
	self assert: ref resolve identicalTo: ref
]

{ #category : #tests }
FileReferenceTest >> testRootParent [
	| root |
	root := filesystem root.
	self assert: root parent identicalTo: root
]

{ #category : #tests }
FileReferenceTest >> testRootReference [
	"Check that the root entry contains sensible values.
	On Windows this requires a pseudo entry to be available."

	| rootReference |

	rootReference := FileReference fileSystem: FileSystem disk path: Path root.

	self assert: rootReference isDirectory.
	self deny: rootReference isFile.
	self assert: rootReference size equals: 0.
	self assert: rootReference isReadable.
	self assert: rootReference isExecutable.
	self deny: rootReference isWritable.

]

{ #category : #tests }
FileReferenceTest >> testSiblingOfReference [
	| griffle  nurb |
	griffle := filesystem / 'griffle'.
	nurb := filesystem / 'nurb'.
	self deny: (griffle isChildOf: nurb).
	self deny: (nurb isChildOf: griffle).
]

{ #category : #tests }
FileReferenceTest >> testSimpleResolution [
	| base relative absolute |
	base := filesystem / 'plonk'.
	relative := (Path * 'griffle') / 'zonk'.
	absolute := base resolve: relative.
	self assert: absolute isAbsolute.
	self assert: (absolute path at: 1) equals: 'plonk'.
	self assert: (absolute path at: 2) equals: 'griffle'.
	self assert: (absolute path at: 3) equals: 'zonk'.
	
	
]

{ #category : #tests }
FileReferenceTest >> testSlash [
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref / 'griffle'.
	self assert: result class equals: ref class.
	self assert: result  isRelative.
	self assert: (result path at: 1) equals: 'plonk'.
	self assert: (result path at: 2) equals: 'griffle'.

]

{ #category : #tests }
FileReferenceTest >> testTempFilePrefixSuffix [
	| fileRef |
	fileRef := FileReference newTempFilePrefix: 'FileReference' suffix: 'Test'.
	self assert: (fileRef isKindOf: FileReference).
	self assert: fileRef exists not.
]

{ #category : #tests }
FileReferenceTest >> testUnequalContent [
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem * 'griffle'.
	self deny: a equals: b
]

{ #category : #tests }
FileReferenceTest >> testUnequalSize [
	| a b |
	a := filesystem * 'plonk'.
	b := filesystem / 'plonk' / 'griffle'.
	self deny: a equals: b
]

{ #category : #tests }
FileReferenceTest >> testUpToAll [
	| testString filename  |
	testString := 'ße'.
	filename := 'test-file-' , 99 atRandom printString , '.txt'.

	[  
	filename asFileReference
		writeStreamEncoded: 'utf8' do: [ :stream | 
			stream nextPutAll: testString ].

	filename asFileReference
		readStreamDo: [ :stream |
			self assert: (stream upToAll: 'e') equals: 'ß'.
		] ]
	ensure: [ filename asFileReference ensureDelete ] 
]

{ #category : #'tests - handy utils' }
FileReferenceTest >> testVersionNumberForComplexFileName [
	
	| dir |
	[
		dir := filesystem / 'dir'.
		(dir / 'test?*file.ston') ensureCreateFile.
	
		self assert: (dir versionNumberFor: 'test?*file' extension: 'ston') equals: 0
	
	] 	ensure: [ (filesystem / 'dir') ensureDeleteAll ].
]

{ #category : #'tests - handy utils' }
FileReferenceTest >> testVersionNumberForExtension [
	
	| childFiles dir |

	[ filesystem createDirectory: 'dir'.
	dir := filesystem / 'dir'.
	dir / 'games.51.ston' writeStreamDo: [ :stream | stream nextPutAll: '51' ].
	dir / 'games.3.ston' writeStreamDo: [ :stream | stream nextPutAll: '3' ].
	dir / 'games.33.ston' writeStreamDo: [ :stream | stream nextPutAll: '33' ].
	dir / 'games.4.ston' writeStreamDo: [ :stream | stream nextPutAll: '4' ].
	dir / 'games.101.ston' writeStreamDo: [ :stream | stream nextPutAll: '101' ].
	dir / 'games.49.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	dir / 'games.5.ston' writeStreamDo: [ :stream | stream nextPutAll: '5' ].
	
	childFiles := dir files.
	self assert: childFiles size equals: 7.
	self assert: (dir versionNumberFor: 'games' extension: 'ston') equals: 101 ]
		ensure: [ (filesystem / 'dir') ensureDeleteAll ].


	[ filesystem createDirectory: 'dir'.
	dir := filesystem / 'dir'.
	dir / 'foo.bar.51.ston' writeStreamDo: [ :stream | stream nextPutAll: '51' ].
	dir / 'foo.bar.3.ston' writeStreamDo: [ :stream | stream nextPutAll: '3' ].
	dir / 'foo.bar.33.ston' writeStreamDo: [ :stream | stream nextPutAll: '33' ].
	dir / 'foo.bar.4.ston' writeStreamDo: [ :stream | stream nextPutAll: '4' ].
	dir / 'foo.bar.101.ston' writeStreamDo: [ :stream | stream nextPutAll: '101' ].
	dir / 'foo.bar.49.ston' writeStreamDo: [ :stream | stream nextPutAll: '49' ].
	dir / 'foo.bar.5.ston' writeStreamDo: [ :stream | stream nextPutAll: '5' ].
	
	childFiles := dir files.
	self assert: childFiles size equals: 7.
	self assert: (dir versionNumberFor: 'foo.bar' extension: 'ston') equals: 101 ]
		ensure: [ (filesystem / 'dir') ensureDeleteAll ].

]

{ #category : #tests }
FileReferenceTest >> testWithExtentionAddsExtension [
	| ref result |
	ref := filesystem * 'plonk'.
	result := ref withExtension: 'griffle'.
	self assert: result isRelative.
	self assert: result basename equals: 'plonk.griffle'
]

{ #category : #tests }
FileReferenceTest >> testWithExtentionReplacesExtension [
	| ref result |
	ref := filesystem * 'plonk.griffle'.
	result := ref withExtension: 'nurp'.
	self assert: result isRelative.
	self assert: result basename equals: 'plonk.nurp'
]

{ #category : #tests }
FileReferenceTest >> testWithoutExtension [
	| ref newRef |
	ref := filesystem * 'plonk' / 'griffle.txt'.
	newRef := ref withoutExtension.
	self assert: newRef parent equals: ref parent.
	self assert: newRef basename equals: 'griffle'
]

{ #category : #tests }
FileReferenceTest >> testWorkingDirectoryParent [
	| wd |
	wd := filesystem referenceTo: Path workingDirectory.
	self assert: wd parent path size equals: 1.
	self assert: (wd parent path at: 1) equals: '..'.
]

{ #category : #tests }
FileReferenceTest >> testWriteStream [
	| ref stream |
	ref := filesystem / 'plonk'.
	[ [ stream := ref writeStream ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testWriteStreamDo [
	| ref s |
	ref := filesystem / 'plonk'.
	[ ref
		writeStreamDo: [ :stream | 
			s := stream.
			self deny: stream isNil ] ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testWriteStreamDoExists [
	| ref path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref writeStreamDo: [ :stream | self deny: stream isNil ] ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testWriteStreamDoifPresent [
	| ref s |
	ref := filesystem / 'plonk'.
	[ ref
		writeStreamDo: [ :stream | 
			s := stream.
			self deny: stream isNil ]
		ifPresent: [ self signalFailure: 'The file does not exist!' ] ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testWriteStreamDoifPresentNot [
	| ref pass path |
	pass := false.
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref writeStreamDo: [ :stream |  ] ifPresent: [ pass := true ].
	self assert: pass ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testWriteStreamExists [
	| ref stream path |
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ [ stream := ref writeStream ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testWriteStreamifPresent [
	| ref stream |
	ref := filesystem / 'plonk'.
	[ [ stream := ref writeStreamIfPresent: [ self signalFailure: 'Should not reach here' ] ]
		ensure: [ stream ifNotNil: [ stream close ] ] ]
		ensure: [ ref ensureDelete ]
]

{ #category : #tests }
FileReferenceTest >> testWriteStreamifPresentExists [
	| ref pass path |
	pass := false.
	path := Path * 'plonk'.
	filesystem store createFile: path.
	ref := filesystem referenceTo: path.
	[ ref writeStreamIfPresent: [ pass := true ].
	self assert: pass ]
		ensure: [ ref ensureDelete ]
]
