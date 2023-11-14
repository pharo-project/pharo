Class {
	#name : 'MCSnapshotTest',
	#superclass : 'MCTestCase',
	#instVars : [
		'snapshot'
	],
	#category : 'Monticello-Tests-Snapshots',
	#package : 'Monticello-Tests',
	#tag : 'Snapshots'
}

{ #category : 'running' }
MCSnapshotTest >> createSnapshotWithClassWithClassSideTraits [

	| t1 t2 |
	self packageOrganizer ensurePackage: 'Monticello-Snapshot-Mock'.

	t1 := self class classInstaller make: [ :aBuilder |
		      aBuilder
			      name: #T1;
			      package: 'Monticello-Snapshot-Mock';
			      beTrait ].

	t2 := self class classInstaller make: [ :aBuilder |
		      aBuilder
			      name: #T2;
			      package: 'Monticello-Snapshot-Mock';
			      beTrait ].

	self class classInstaller make: [ :aBuilder |
		aBuilder
			name: #C1;
			traitComposition: t1 + t2;
			classTraitComposition: t1 classTrait + t2 classTrait - #m1;
			package: 'Monticello-Snapshot-Mock' ].

	^ (MCWorkingCopy forPackageNamed: 'Monticello-Snapshot-Mock') snapshot
]

{ #category : 'running' }
MCSnapshotTest >> createSnapshotWithTraitWithClassSideTraits [

	| t1 t2 t3 |
	self packageOrganizer ensurePackage: 'Monticello-Snapshot-Mock'.

	t1 := self class classInstaller make: [ :aBuilder |
		      aBuilder
			      name: #T1;
			      package: 'Monticello-Snapshot-Mock';
			      beTrait ].

	t2 := self class classInstaller make: [ :aBuilder |
		      aBuilder
			      name: #T2;
			      package: 'Monticello-Snapshot-Mock';
			      beTrait ].

	t3 := self class classInstaller make: [ :aBuilder |
		      aBuilder
			      name: #T3;
			      traitComposition: t1 + t2;
			      classTraitComposition: t1 classTrait + t2 classTrait - #m1;
			      package: 'Monticello-Snapshot-Mock';
			      beTrait ].

	^ (MCWorkingCopy forPackageNamed: 'Monticello-Snapshot-Mock') snapshot
]

{ #category : 'utilities' }
MCSnapshotTest >> lookupClass: aName in: definitions [
	^ definitions
		detect: [ :aDef | aDef isClassDefinition and: [ aDef className = aName ] ]
]

{ #category : 'utilities' }
MCSnapshotTest >> lookupTrait: aName in: definitions [ 

	^ definitions detect: [ :aDef | aDef isTraitDefinition and: [ aDef className = aName ] ]
]

{ #category : 'running' }
MCSnapshotTest >> setUp [
	super setUp.
	snapshot :=  self mockSnapshot.
]

{ #category : 'running' }
MCSnapshotTest >> tearDown [ 

	self packageOrganizer removePackage: 'Monticello-Snapshot-Mock'.
	super tearDown.
]

{ #category : 'tests' }
MCSnapshotTest >> testClassWithClassTraitCompositionHasCorrectTraitComposition [

	| definitions def |
	definitions :=  self createSnapshotWithClassWithClassSideTraits definitions.
	def := self lookupClass: #C1 in: definitions.
	
	self assert: def classTraitComposition equals: '(T1 classTrait + T2 classTrait) - {#m1}'


]

{ #category : 'tests' }
MCSnapshotTest >> testClassWithClassTraitCompositionHasTraitComposition [

	| definitions def |
	definitions :=  self createSnapshotWithClassWithClassSideTraits definitions.
	def := self lookupClass: #C1 in: definitions.
	
	self assert: def hasClassTraitComposition.



]

{ #category : 'tests' }
MCSnapshotTest >> testCreation [
	|d|
	d :=  self mockSnapshot definitions.
	self assert: (d anySatisfy: [:ea | ea isClassDefinition and: [ea className = #MCMockClassA]]).
	self assert: (d anySatisfy: [:ea | ea isMethodDefinition and: [ea selector = #mockClassExtension]]).
	self assert: (d allSatisfy: [:ea | ea isClassDefinition not or: [ea category endsWith: 'Mocks']]).
	
]

{ #category : 'tests' }
MCSnapshotTest >> testTraitWithClassTraitCompositionHasCorrectTraitComposition [

	| definitions def |
	definitions :=  self createSnapshotWithTraitWithClassSideTraits definitions.
	def := self lookupTrait: #T3 in: definitions.
	
	self assert: def classTraitComposition equals: '(T1 classTrait + T2 classTrait) - {#m1}'


]

{ #category : 'tests' }
MCSnapshotTest >> testTraitWithClassTraitCompositionHasTraitComposition [

	| definitions def |
	definitions := self createSnapshotWithTraitWithClassSideTraits definitions.
	"This could use #lookupTrait:in: when this issue will be fixed: https://github.com/pharo-project/pharo/issues/14605
	
	The problem is that since MCClassTraitDefinition returns true, we can get a MCClassTraitDefinition instead of a MCTraitDefinition"
	def := definitions detect: [ :aDef | (aDef isKindOf: MCTraitDefinition) and: [ aDef className = #T3 ] ].

	self assert: def hasClassTraitComposition
]
