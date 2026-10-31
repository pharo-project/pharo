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
	self assert: e1 loadedInSessions isEmpty.

	repository recordLoadOf: e1.
	self assert: e1 loadCount equals: 1.
	self assert: (e1 loadedInSessions includes: repository currentSessionMarker)
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testVisibleEntriesShowsManualEntries [

	| e1 e2 |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	e1 isManual: true.
	e2 := repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2'.

	self assert: (repository visibleEntries includes: e1).
	self deny: (repository visibleEntries includes: e2)
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testVisibleEntriesShowsRecentLoadedEntries [

	| e1 e2 |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	e2 := repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2'.

	repository recordLoadOf: e1.

	self assert: (repository visibleEntries includes: e1).
	self deny: (repository visibleEntries includes: e2)
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testVisibleEntriesHidesOldSessions [

	| e1 e2 recentMarker oldMarker |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	e2 := repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2'.

	oldMarker := DateAndTime now - 10 days.
	recentMarker := DateAndTime now.
	QuickProjectRepository recentImagesWindowSize: 1.
	e1 recordLoadInSession: oldMarker.
	e2 recordLoadInSession: recentMarker.

	self deny: (repository visibleEntries includes: e1).
	self assert: (repository visibleEntries includes: e2)
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testRecentSessionMarkersLimitedToWindow [

	| e1 e2 |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	e2 := repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2'.

	QuickProjectRepository recentImagesWindowSize: 2.
	e1 recordLoadInSession: (DateAndTime now - 10 days).
	e2 recordLoadInSession: (DateAndTime now - 9 days).
	e1 recordLoadInSession: DateAndTime now.
	e2 recordLoadInSession: DateAndTime now.

	self assert: repository recentSessionMarkers size equals: 2
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testVisibleEntriesOrdersRecentFirst [

	| e1 e2 e3 |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	e2 := repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2'.
	e3 := repository addEntryNamed: 'Proj 3' repository: 'github://a/3' baseline: 'P3'.

	e2 recordLoadInSession: (DateAndTime now - 2 days).
	e3 recordLoadInSession: (DateAndTime now - 1 day).

	self assert: (repository visibleEntries asArray) equals: { e3. e2 }
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
	(repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1') isManual: true.
	(repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2') isManual: true.
	repository save.

	self assert: tempFile exists.

	loadedRepo := QuickProjectRepository new.
	loadedRepo persistencePath: tempFile.
	loadedRepo load.

	self assert: loadedRepo rawEntries size equals: 2.
	self assert: loadedRepo rawEntries first name equals: 'Proj 1'.
	self assert: loadedRepo rawEntries second name equals: 'Proj 2'
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testAddingManualEntryPromotesExistingOne [

	| recorded |
	recorded := repository addEntryNamed: 'Proj' repository: 'github://a/b' baseline: 'P'.
	repository addEntry: (QuickProjectEntry new name: 'Proj'; isManual: true; yourself).

	self assert: recorded isManual
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testCorruptFileKeepsEntries [

	| entry |
	entry := repository addEntryNamed: 'Proj' repository: 'github://a/b' baseline: 'P'.
	tempFile ensureDelete.
	tempFile writeStreamDo: [ :stream | stream nextPutAll: '[ not ston' ].

	repository load.

	self assert: (repository rawEntries includes: entry)
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testLoadDropsGuessedEntries [
	"Older versions filled the file with every BaselineOf in the image, never loaded and with invented URLs."

	| reader |
	repository addEntryNamed: 'Guessed' repository: 'github://pharo-project/Guessed' baseline: 'Guessed'.
	repository recordLoadOf: (repository addEntryNamed: 'Real' repository: 'github://a/real' baseline: 'Real').

	reader := QuickProjectRepository new persistencePath: tempFile; yourself.
	reader load.

	self assert: (reader rawEntries collect: [ :each | each name ]) asArray equals: #( 'Real' )
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testMutationKeepsChangesFromOtherImages [
	"The file is shared between images: nobody may overwrite it from a stale copy."

	| otherImage |
	repository recordLoadOf: (repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1').

	otherImage := QuickProjectRepository new persistencePath: tempFile; yourself.
	otherImage syncWithDisk.
	otherImage recordLoadOf: (QuickProjectEntry name: 'Proj 2' repositoryUrl: 'github://a/2' baseline: 'P2').

	repository recordLoadOf: (QuickProjectEntry name: 'Proj 3' repositoryUrl: 'github://a/3' baseline: 'P3').

	self
		assertCollection: (repository rawEntries collect: [ :each | each name ])
		hasSameElements: #( 'Proj 1' 'Proj 2' 'Proj 3' )
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testRecordExecutionIgnoresFailedLoad [

	| executor |
	executor := MetacelloScriptExecutor new
		baselineArg: 'SurelyNotInThisImage';
		repositoryArg: 'github://a/b';
		yourself.
	executor load: #(  ).

	repository recordExecutionOf: executor.

	self deny: repository hasEntries
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testRecordExecutionIgnoresOtherActions [

	| executor |
	executor := MetacelloScriptExecutor new
		baselineArg: 'Metacello';
		repositoryArg: 'github://a/b';
		yourself.
	executor get.

	repository recordExecutionOf: executor.

	self deny: repository hasEntries
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testRecordExecutionRemembersLoad [

	| executor entry |
	executor := MetacelloScriptExecutor new
		baselineArg: 'BaselineOfMetacello';
		repositoryArg: 'github://a/b:main/src';
		yourself.
	executor load: #( 'Core' ).

	repository recordExecutionOf: executor.

	entry := repository visibleEntries first.
	self assert: entry baselineName equals: 'Metacello'.
	self assert: entry repositoryUrl equals: 'github://a/b:main/src'.
	self assert: entry groups asArray equals: #( 'Core' )
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testRecordLoadCreatesMissingEntry [

	repository recordLoadOf: (QuickProjectEntry name: 'Proj' repositoryUrl: 'github://a/b' baseline: 'P').

	self assert: repository visibleEntries size equals: 1
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testSaveShorterListLeavesValidFile [

	| e1 reader |
	e1 := repository addEntryNamed: 'Proj 1' repository: 'github://a/1' baseline: 'P1'.
	(repository addEntryNamed: 'Proj 2' repository: 'github://a/2' baseline: 'P2') isManual: true.
	e1 isManual: true.
	repository save.

	repository removeEntry: e1.

	reader := QuickProjectRepository new persistencePath: tempFile; yourself.
	reader load.
	self assert: (reader rawEntries collect: [ :each | each name ]) asArray equals: #( 'Proj 2' )
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testSessionMarkerIsStableWithinASession [

	self assert: repository currentSessionMarker equals: repository currentSessionMarker
]

{ #category : 'tests' }
QuickProjectRepositoryTest >> testSessionMarkersSurviveTheFile [

	| reader |
	repository recordLoadOf: (repository addEntryNamed: 'Proj' repository: 'github://a/b' baseline: 'P').

	reader := QuickProjectRepository new persistencePath: tempFile; yourself.
	reader load.

	self assert: reader visibleEntries size equals: 1.
	self assert: reader visibleEntries first loadedInSessions first equals: repository currentSessionMarker
]
