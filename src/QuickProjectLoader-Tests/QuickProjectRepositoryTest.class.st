"
Unit tests for QuickProjectRepository persistence and management.
"
Class {
	#name : 'QuickProjectRepositoryTest',
	#superclass : 'TestCase',
	#instVars : [
		'repository',
		'tempFile'
	],
	#category : 'QuickProjectLoader-Tests',
	#package : 'QuickProjectLoader-Tests'
}

{ #category : 'running' }
QuickProjectRepositoryTest >> setUp [

	super setUp.
	tempFile := FileSystem memory / 'test_quick_projects.ston'.
	repository := QuickProjectRepository new.
	repository persistencePath: tempFile
]

{ #category : 'running' }
QuickProjectRepositoryTest >> tearDown [

	QuickProjectRepository reset.
	tempFile deleteIfAbsent: [].
	super tearDown
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testAddDuplicateEntry [

	| entry1 entry2 |
	entry1 := QuickProjectEntry name: 'Proj' repositoryUrl: 'github://a/b' baseline: 'P'.
	entry2 := QuickProjectEntry name: 'Proj' repositoryUrl: 'github://a/b' baseline: 'P'.

	repository addEntry: entry1.
	repository addEntry: entry2.

	self assert: repository rawEntries size equals: 1
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testDeduplicationByName [

	| entry1 entry2 |
	entry1 := QuickProjectEntry name: 'Roassal' repositoryUrl: 'github://ObjectProfile/Roassal/src' baseline: 'Roassal'.
	entry2 := QuickProjectEntry name: 'Roassal' repositoryUrl: 'github://pharo-project/Roassal' baseline: 'Roassal'.

	repository addEntry: entry1.
	repository addEntry: entry2.

	self assert: repository entries size equals: 1
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testAddEntry [

	self deny: repository hasEntries.
	repository addEntryNamed: 'Proj 1' repository: 'github://a/b' baseline: 'P1'.
	self assert: repository hasEntries.
	self assert: repository rawEntries size equals: 1.
	self assert: repository rawEntries first name equals: 'Proj 1'
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testEntriesSortedByLoadCount [

	| e1 e2 e3 |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	e2 := repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2'.
	e3 := repository addEntryNamed: 'Proj 3' repository: 'github://a/3' baseline: 'P3'.

	repository recordLoadOf: e2.
	repository recordLoadOf: e2.

	repository recordLoadOf: e3.
	repository recordLoadOf: e3.
	repository recordLoadOf: e3.

	self assert: repository entries asArray equals: { e3. e2. e1 }
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testHasEntries [

	self deny: repository hasEntries.
	repository addEntryNamed: 'P' repository: 'github://a/b' baseline: 'P'.
	self assert: repository hasEntries
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testInferProjectsDoesNotCrash [

	repository inferProjects.
	"May or may not infer projects depending on image state, but must run without errors"
	self assert: true
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testLoadFromMissingFile [

	self deny: tempFile exists.
	repository load.
	self deny: repository hasEntries
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testMoveEntryToIndex [

	| e1 e2 e3 |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	e2 := repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2'.
	e3 := repository addEntryNamed: 'Proj 3' repository: 'github://a/3' baseline: 'P3'.

	repository moveEntry: e3 toIndex: 1.
	self assert: repository rawEntries first equals: e3
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testRecordLoad [

	| e1 |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	self assert: e1 loadCount equals: 0.

	repository recordLoadOf: e1.
	self assert: e1 loadCount equals: 1
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testRemoveEntry [

	| e1 e2 |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	e2 := repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2'.

	self assert: repository rawEntries size equals: 2.
	repository removeEntry: e1.
	self assert: repository rawEntries size equals: 1.
	self assert: repository rawEntries first equals: e2
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testSaveAndLoad [

	| loadedRepo |
	repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2'.

	self assert: tempFile exists.

	loadedRepo := QuickProjectRepository new.
	loadedRepo persistencePath: tempFile.
	loadedRepo load.

	self assert: loadedRepo rawEntries size equals: 2.
	self assert: loadedRepo rawEntries first name equals: 'Proj 1'.
	self assert: loadedRepo rawEntries second name equals: 'Proj 2'
]
