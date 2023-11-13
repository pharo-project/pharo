Class {
	#name : 'MCWorkingCopyTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'workingCopy',
		'repositoryGroup',
		'versions',
		'versions2',
		'savedName'
	],
	#category : 'Monticello-Tests-Base',
	#package : 'Monticello-Tests',
	#tag : 'Base'
}

{ #category : 'asserting' }
MCWorkingCopyTest >> assertNameWhenSavingTo: aRepository is: aString [
	| name |
	name := nil.
	[ aRepository storeVersion: (workingCopy newVersionIn: aRepository) ]
		on: MCVersionNameAndMessageRequest
		do: [ :n | 
			name := n suggestedName.
			n resume: (Array with: name with: '') ].
	self assert: name equals: aString
]

{ #category : 'asserting' }
MCWorkingCopyTest >> assertNumberWhenSavingTo: aRepository is: aNumber [
	| name |
	name := nil.
	[ aRepository storeVersion: (workingCopy newVersionIn: aRepository) ]
		on: MCVersionNameAndMessageRequest
		do: [ :n | 
			name := n suggestedName.
			n resume: (Array with: name with: '') ].
	self assert: name equals: self packageName , '-' , Author fullName , '.' , aNumber asString
]

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
	workingCopy repositoryGroup: repositoryGroup.
	savedName := Author fullName.
	Author fullName: 'abc'
]

{ #category : 'actions' }
MCWorkingCopyTest >> snapshot [
	| version |
	[version := workingCopy newVersionIn: workingCopy repositoryGroup]
		on: MCVersionNameAndMessageRequest
		do: [:n | n resume: (Array with: n suggestedName with: '')].
	versions at: version info put: version.
	^ version
]

{ #category : 'running' }
MCWorkingCopyTest >> tearDown [

	workingCopy unregister.
	self restoreMocks.
	Author fullName: savedName.
	super tearDown
]

{ #category : 'tests' }
MCWorkingCopyTest >> testAncestorMerge [
	| base revA revB revC |

	base := self snapshot.
	self change: #a toReturn: 'a1'.
	revA :=  self snapshot.
	self change: #b toReturn: 'b1'.
	revB :=  self snapshot.	
	self change: #c toReturn: 'c1'.
	revC :=  self snapshot.	

	self should: [self basicMerge: revA] raise: MCNoChangesException.
	
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
MCWorkingCopyTest >> testNaming [
	| repos version |

	repos := MCDictionaryRepository new.
	self assertNameWhenSavingTo: repos is: self packageName, '-abc.1'.
	self assertNameWhenSavingTo: repos is: self packageName, '-abc.2'.
	repos := MCDictionaryRepository new.
	self assertNameWhenSavingTo: repos is: self packageName, '-abc.3'.
	version := self snapshot.
	version info instVarNamed: 'name' put: 'foo-jf.32'.
	version load.
	self assertNameWhenSavingTo: repos is: 'foo-abc.33'.
	self assertNameWhenSavingTo: repos is: 'foo-abc.34'.
	version info instVarNamed: 'name' put: 'foo-abc.35'.
	repos storeVersion: version.
	self assertNameWhenSavingTo: repos is: 'foo-abc.36'.
	self assertNameWhenSavingTo: repos is: 'foo-abc.37'.
	version info instVarNamed: 'name' put: 'foo-abc.10'.
	repos storeVersion: version.
	self assertNameWhenSavingTo: repos is: 'foo-abc.38'.
	version info instVarNamed: 'name' put: 'foo2-ab.40'.
	version load.
	self assertNameWhenSavingTo: repos is: 'foo2-abc.41'.
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
