Class {
	#name : #MCSnapshotTest,
	#superclass : #MCTestCase,
	#instVars : [
		'snapshot'
	],
	#category : #'Monticello-Tests-Snapshots'
}

{ #category : #running }
MCSnapshotTest >> createSnapshotWithClassWithClassSideTraits [

	| t1 t2 c1 |
	
	RPackageOrganizer default registerPackageNamed: 'Monticello-Snapshot-Mock'.
	
	t1 := Trait named: #T1 uses: {} package: 'Monticello-Snapshot-Mock'.
	t2 := Trait named: #T2 uses: {} package: 'Monticello-Snapshot-Mock'.	

	c1 := Object 
		subclass: #C1 
		uses: t1 + t2 
		slots: #() 
		classVariables: #() 
		package: 'Monticello-Snapshot-Mock'.

	c1 class traitComposition: (t1 classTrait + t2 classTrait - #m1).
	
	^ (MCWorkingCopy forPackage: (MCPackage new name: 'Monticello-Snapshot-Mock')) snapshot. 
]

{ #category : #running }
MCSnapshotTest >> createSnapshotWithTraitWithClassSideTraits [

	| t1 t2 t3 |
	
	RPackageOrganizer default registerPackageNamed: 'Monticello-Snapshot-Mock'.
	
	t1 := Trait named: #T1 uses: {} package: 'Monticello-Snapshot-Mock'.
	t2 := Trait named: #T2 uses: {} package: 'Monticello-Snapshot-Mock'.	
	t3 := Trait named: #T3 uses: t1 + t2 package: 'Monticello-Snapshot-Mock'.
	t3 classTrait traitComposition: (t1 classTrait + t2 classTrait - #m1).
	
	^ (MCWorkingCopy forPackage: (MCPackage new name: 'Monticello-Snapshot-Mock')) snapshot. 
]

{ #category : #utils }
MCSnapshotTest >> lookupClass: aName in: definitions [
	^ definitions
		detect: [ :aDef | aDef isClassDefinition and: [ aDef className = aName ] ]
]

{ #category : #utils }
MCSnapshotTest >> lookupTrait: aName in: definitions [ 

	^ definitions detect: [ :aDef | aDef isTraitDefinition and: [ aDef className = aName ] ]
]

{ #category : #running }
MCSnapshotTest >> setUp [
	super setUp.
	snapshot :=  self mockSnapshot.
]

{ #category : #running }
MCSnapshotTest >> tearDown [ 

	| package |
	package := 'Monticello-Snapshot-Mock' asPackageIfAbsent: [nil].
	package ifNotNil: [ package removeFromSystem ].
	super tearDown.
]

{ #category : #tests }
MCSnapshotTest >> testClassWithClassTraitCompositionHasCorrectTraitComposition [

	| definitions def |
	definitions :=  self createSnapshotWithClassWithClassSideTraits definitions.
	def := self lookupClass: #C1 in: definitions.
	
	self assert: def classTraitComposition equals: '(T1 classTrait + T2 classTrait) - {#m1}'


]

{ #category : #tests }
MCSnapshotTest >> testClassWithClassTraitCompositionHasTraitComposition [

	| definitions def |
	definitions :=  self createSnapshotWithClassWithClassSideTraits definitions.
	def := self lookupClass: #C1 in: definitions.
	
	self assert: def hasClassTraitComposition.



]

{ #category : #tests }
MCSnapshotTest >> testCreation [
	|d|
	d :=  self mockSnapshot definitions.
	self assert: (d anySatisfy: [:ea | ea isClassDefinition and: [ea className = #MCMockClassA]]).
	self assert: (d anySatisfy: [:ea | ea isMethodDefinition and: [ea selector = #mockClassExtension]]).
	self assert: (d allSatisfy: [:ea | ea isClassDefinition not or: [ea category endsWith: 'Mocks']]).
	
]

{ #category : #tests }
MCSnapshotTest >> testTraitWithClassTraitCompositionHasCorrectTraitComposition [

	| definitions def |
	definitions :=  self createSnapshotWithTraitWithClassSideTraits definitions.
	def := self lookupTrait: #T3 in: definitions.
	
	self assert: def classTraitComposition equals: '(T1 classTrait + T2 classTrait) - {#m1}'


]

{ #category : #tests }
MCSnapshotTest >> testTraitWithClassTraitCompositionHasTraitComposition [

	| definitions def |
	definitions :=  self createSnapshotWithTraitWithClassSideTraits definitions.
	def := self lookupTrait: #T3 in: definitions.
	
	self assert: def hasClassTraitComposition.



]
