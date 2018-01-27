Class {
	#name : #MCWorkingCopyTest,
	#superclass : #MCTestCase,
	#instVars : [
		'workingCopy',
		'repositoryGroup',
		'versions',
		'versions2',
		'savedName'
	],
	#category : #'Monticello-Tests-Base'
}

{ #category : #asserting }
MCWorkingCopyTest >> assertNameWhenSavingTo: aRepository is: aString [
	| name |
	name := nil.
	[aRepository storeVersion: (workingCopy newVersionIn: aRepository)]
		on: MCVersionNameAndMessageRequest
		do: [:n | name := n suggestedName. n resume: (Array with: name with: '')].
	self assert: name = aString
]

{ #category : #asserting }
MCWorkingCopyTest >> assertNumberWhenSavingTo: aRepository is: aNumber [
	| name |
	name := nil.
	[aRepository storeVersion: (workingCopy newVersionIn: aRepository)]
		on: MCVersionNameAndMessageRequest
		do: [:n | name := n suggestedName. n resume: (Array with: name with: '')].
	self assert: name = (self packageName, '-', Author fullName, '.', aNumber asString)
]

{ #category : #actions }
MCWorkingCopyTest >> basicMerge: aVersion [
	aVersion merge
]

{ #category : #running }
MCWorkingCopyTest >> clearPackageCache [
	| dir |
	dir := MCCacheRepository uniqueInstance directory.
"	(dir filesMatching: 'MonticelloMocks*') do: [:ea | ea ensureDeleted ]."
	(dir filesMatching: 'MonticelloTest*') do: [:ea | ea ensureDelete].
	(dir filesMatching: 'rev*') do: [:ea | ea ensureDelete].
	(dir filesMatching: 'foo-*') do: [:ea | ea ensureDelete].
	(dir filesMatching: 'foo2-*') do: [:ea | ea ensureDelete].
]

{ #category : #accessing }
MCWorkingCopyTest >> description [
	^ self class name
]

{ #category : #actions }
MCWorkingCopyTest >> load: aVersion [
	aVersion load
]

{ #category : #actions }
MCWorkingCopyTest >> merge: aVersion [
	[[self basicMerge: aVersion]
		on: MCMergeResolutionRequest do: [:n | n merge ]]
			on: MCNoChangesException do: [:n | ]
]

{ #category : #private }
MCWorkingCopyTest >> packageName [
	^ self mockPackage name
]

{ #category : #running }
MCWorkingCopyTest >> setUp [
	| repos1 repos2 |
	super setUp.
	self clearPackageCache.
	repositoryGroup := MCRepositoryGroup new.
	repositoryGroup disableCache.
	workingCopy := MCWorkingCopy forPackage: self mockPackage.
	versions := Dictionary new.
	versions2 := Dictionary new.
	repos1 := MCDictionaryRepository new dictionary: versions.
	repos2 := MCDictionaryRepository new dictionary: versions2.
	repositoryGroup addRepository: repos1.
	repositoryGroup addRepository: repos2.
	MCRepositoryGroup default removeRepository: repos1; removeRepository: repos2.
	workingCopy repositoryGroup: repositoryGroup.
	savedName := Author fullName.
	Author fullName: 'abc'.
]

{ #category : #actions }
MCWorkingCopyTest >> snapshot [
	| version |
	[version := workingCopy newVersionIn: workingCopy repositoryGroup]
		on: MCVersionNameAndMessageRequest
		do: [:n | n resume: (Array with: n suggestedName with: '')].
	versions at: version info put: version.
	^ version
]

{ #category : #running }
MCWorkingCopyTest >> tearDown [
	workingCopy unregister.
	self restoreMocks.
	self clearPackageCache.
	Author fullName: savedName.
	super tearDown
]

{ #category : #tests }
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

{ #category : #tests }
MCWorkingCopyTest >> testBackport [
	| inst base final backported ancestry |
	inst := self mockInstanceA.
	base := self snapshot.
	
	self assert: inst one = 1.
	
	self change: #one toReturn: 2.
	self change: #two toReturn: 3.
	
	final := self snapshot.
	[workingCopy backportChangesTo: base info]
		on: MCChangeSelectionRequest
		do: [:e | e resume: e patch].
		
	self assert: inst one = 2.
	self assert: inst two = 3.
	ancestry := workingCopy ancestry.
	self assert: ancestry ancestors size = 1.
	self assert: ancestry ancestors first = base info.
	self assert: ancestry stepChildren size = 1.
	self assert: ancestry stepChildren first = final info.
	
	backported := self snapshot.
	[workingCopy backportChangesTo: base info]
		on: MCChangeSelectionRequest
		do: [:e | e resume: e patch].
		
	ancestry := workingCopy ancestry.
	self assert: ancestry ancestors size = 1.
	self assert: ancestry ancestors first = base info.
	self assert: ancestry stepChildren size = 1.
	self assert: ancestry stepChildren first = backported info.
	
]

{ #category : #tests }
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
	self assert: inst a = 'a2'.
	self assert: inst b = 'b2'.
	self assert: inst c = 'c1'.
	self assert: inst d = 'd1'
]

{ #category : #tests }
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

	self assert: (workingCopy ancestors size = 2)
	
]

{ #category : #tests }
MCWorkingCopyTest >> testMergeIntoUnmodifiedImage [
	| base revA |

	base := self snapshot.
	self change: #a toReturn: 'a1'.
	revA := self snapshot.
	
	self load: base.

	self merge: revA.

	self assert: (workingCopy ancestors size = 1)
	
]

{ #category : #tests }
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

{ #category : #tests }
MCWorkingCopyTest >> testOptimizedLoad [
	| inst base diffy |
	inst := self mockInstanceA.
	base := self snapshot.
	self change: #one toReturn: 2.
	self assert: inst one = 2.
	diffy := self snapshot asDiffAgainst: base.
	self deny: diffy canOptimizeLoading.
	self load: base.
	self assert: inst one = 1.
	self assert: diffy canOptimizeLoading.
	self load: diffy.
	self assert: inst one = 2.

]

{ #category : #tests }
MCWorkingCopyTest >> testRedundantMerge [
	| base |
	base := self snapshot.
	self merge: base.
	self merge: base
]

{ #category : #tests }
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
	self assert: inst one = 1.
	self assert: inst two = 2.
	self merge: mother1.
	self assert: inst one = 2.
	self assert: inst two = 2.
	self change: #one toReturn: 7.
	self assert: inst one = 7.
	self assert: inst two = 2.
	self merge: mother2.
	self assert: inst one = 7.
	self assert: inst two = 3
]

{ #category : #tests }
MCWorkingCopyTest >> testRepositoryFallback [
	| version |
	version := self snapshot.
	self assert: (repositoryGroup versionWithInfo: version info) == version.
	versions removeKey: version info.
	versions2 at: version info put: version.
	self assert: ( repositoryGroup versionWithInfo: version info) == version.
	versions2 removeKey: version info.
	self should: [repositoryGroup versionWithInfo: version info] raise: Error.
]

{ #category : #tests }
MCWorkingCopyTest >> testSelectiveBackport [
	| inst base intermediate final |
	inst := self mockInstanceA.
	base :=  self snapshot.
	self assert: inst one = 1.
	self change: #one toReturn: 2.
	intermediate := self snapshot.
	self change: #two toReturn: 3.
	final := self snapshot.
	[workingCopy backportChangesTo: base info]
		on: MCChangeSelectionRequest
		do: [:e | | patch selected |
			patch := e patch.
			selected := patch operations select: [:ea | ea definition selector = #two].
			e resume: (MCPatch operations: selected)]. 
	self assert: inst one = 1.
	self assert: inst two = 3.
	self assert: workingCopy ancestry ancestors size = 1.
	self assert: workingCopy ancestry ancestors first = base info.
	self assert: workingCopy ancestry stepChildren size = 1.
	self assert: workingCopy ancestry stepChildren first = final info
]

{ #category : #tests }
MCWorkingCopyTest >> testSimpleMerge [
	| mother base inst |
	inst := self mockInstanceA.
	base :=  self snapshot.
	self change: #one toReturn: 2.
	mother :=  self snapshot.
	self load: base.
	self change: #two toReturn: 3.
	self snapshot.
	self assert: inst one = 1.
	self assert: inst two = 3.
	
	self merge: mother.
	self assert: inst one = 2.
	self assert: inst two = 3.
]

{ #category : #tests }
MCWorkingCopyTest >> testSnapshotAndLoad [
	| base inst |
	inst := self mockInstanceA.
	base :=  self snapshot.
	self change: #one toReturn: 2.
	self assert: inst one = 2.
	self load: base.
	self assert: inst one = 1.
]
