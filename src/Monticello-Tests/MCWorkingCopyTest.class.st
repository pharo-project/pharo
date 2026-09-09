Class {
	#name : 'MCWorkingCopyTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'workingCopy',
		'repositoryGroup',
		'versions',
		'versions2'
	],
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'actions' }
MCWorkingCopyTest >> basicMerge: aVersion [
	aVersion merge
]

{ #category : 'accessing' }
MCWorkingCopyTest >> description [
	^ self class name
]

{ #category : 'actions' }
MCWorkingCopyTest >> load: aVersion [
	aVersion load
]

{ #category : 'actions' }
MCWorkingCopyTest >> merge: aVersion [
	[[self basicMerge: aVersion]
		on: MCMergeResolutionRequest do: [:n | n merge ]]
			on: MCNoChangesException do: [:n | ]
]

{ #category : 'private' }
MCWorkingCopyTest >> packageName [
	^ self mockPackage name
]

{ #category : 'running' }
MCWorkingCopyTest >> runCase [

	MCCacheRepository uniqueInstance useDirectory: FileSystem memory / 'test' during: [ super runCase ]
]

{ #category : 'running' }
MCWorkingCopyTest >> setUp [

	| repos1 repos2 |
	super setUp.
	repositoryGroup := MCRepositoryGroup new.
	repositoryGroup disableCache.
	workingCopy := MCWorkingCopy ensureForPackage: self mockPackage.
	versions := Dictionary new.
	versions2 := Dictionary new.
	repos1 := MCDictionaryRepository new dictionary: versions.
	repos2 := MCDictionaryRepository new dictionary: versions2.
	repositoryGroup addRepository: repos1.
	repositoryGroup addRepository: repos2.
	MCRepositoryGroup default
		removeRepository: repos1;
		removeRepository: repos2.
	workingCopy repositoryGroup: repositoryGroup
]

{ #category : 'actions' }
MCWorkingCopyTest >> snapshot [

	| version repository |
	repository := workingCopy repositoryGroup.
	version := workingCopy newVersionWithName: (workingCopy uniqueVersionNameIn: repository) message: '' in: repository.
	versions at: version info put: version.
	^ version
]

{ #category : 'running' }
MCWorkingCopyTest >> tearDown [

	workingCopy unregister.
	self restoreMocks.
	super tearDown
]

{ #category : 'tests' }
MCWorkingCopyTest >> testAncestorMerge [

	| base revision |
	base := self snapshot.
	self change: #a toReturn: 'a1'.
	revision := self snapshot.

	self should: [ self basicMerge: revision ] raise: MCNoChangesException
]

{ #category : 'tests' }
MCWorkingCopyTest >> testDoubleRepeatedMerge [
	| base motherA1 motherA2 motherB1 motherB2 inst |
	base := self snapshot.
	self change: #a toReturn: 'a1'.
	motherA1 := self snapshot.
	self change: #c toReturn: 'c1'.
	motherA2 := self snapshot.
	self load: base.
	self change: #b toReturn: 'b1'.
	motherB1 := self snapshot.
	self change: #d toReturn: 'd1'.
	motherB2 := self snapshot.
	self load: base.
	self merge: motherA1.
	self merge: motherB1.
	self change: #a toReturn: 'a2'.
	self change: #b toReturn: 'b2'.
	self snapshot.
	self merge: motherA2.
	self merge: motherB2.
	inst := self mockInstanceA.
	self assert: inst a equals: 'a2'.
	self assert: inst b equals: 'b2'.
	self assert: inst c equals: 'c1'.
	self assert: inst d equals: 'd1'
]

{ #category : 'tests' }
MCWorkingCopyTest >> testMergeIntoImageWithNoChanges [
	| base revB revA1 |
	self change: #a toReturn: 'a'.
	base := self snapshot.
	self change: #b toReturn: 'b'.
	revB := self snapshot.

	self load: base.
	self change: #a toReturn: 'a1'.
	revA1 := self snapshot.

	self change: #a toReturn: 'a'.
	self snapshot.
	self merge: revB.

	self assert: workingCopy ancestors size equals: 2
]

{ #category : 'tests' }
MCWorkingCopyTest >> testMergeIntoUnmodifiedImage [
	| base revA |
	base := self snapshot.
	self change: #a toReturn: 'a1'.
	revA := self snapshot.

	self load: base.

	self merge: revA.

	self assert: workingCopy ancestors size equals: 1
]

{ #category : 'tests' }
MCWorkingCopyTest >> testRedundantMerge [
	| base |
	base := self snapshot.
	self merge: base.
	self merge: base
]

{ #category : 'tests' }
MCWorkingCopyTest >> testRepeatedMerge [
	| base mother1 mother2 inst |
	base := self snapshot.
	self change: #one toReturn: 2.
	mother1 := self snapshot.
	self change: #two toReturn: 3.
	mother2 := self snapshot.
	self load: base.
	self change: #truth toReturn: false.
	self snapshot.
	inst := self mockInstanceA.
	self assert: inst one equals: 1.
	self assert: inst two equals: 2.
	self merge: mother1.
	self assert: inst one equals: 2.
	self assert: inst two equals: 2.
	self change: #one toReturn: 7.
	self assert: inst one equals: 7.
	self assert: inst two equals: 2.
	self merge: mother2.
	self assert: inst one equals: 7.
	self assert: inst two equals: 3
]

{ #category : 'tests' }
MCWorkingCopyTest >> testRepositoryFallback [
	| version |
	version := self snapshot.
	self assert: (repositoryGroup versionWithInfo: version info) identicalTo: version.
	versions removeKey: version info.
	versions2 at: version info put: version.
	self assert: (repositoryGroup versionWithInfo: version info) identicalTo: version.
	versions2 removeKey: version info.
	self should: [ repositoryGroup versionWithInfo: version info ] raise: Error
]

{ #category : 'tests' }
MCWorkingCopyTest >> testSimpleMerge [
	| mother base inst |
	inst := self mockInstanceA.
	base := self snapshot.
	self change: #one toReturn: 2.
	mother := self snapshot.
	self load: base.
	self change: #two toReturn: 3.
	self snapshot.
	self assert: inst one equals: 1.
	self assert: inst two equals: 3.

	self merge: mother.
	self assert: inst one equals: 2.
	self assert: inst two equals: 3
]

{ #category : 'tests' }
MCWorkingCopyTest >> testSnapshotAndLoad [
	| base inst |
	inst := self mockInstanceA.
	base := self snapshot.
	self change: #one toReturn: 2.
	self assert: inst one equals: 2.
	self load: base.
	self assert: inst one equals: 1
]
