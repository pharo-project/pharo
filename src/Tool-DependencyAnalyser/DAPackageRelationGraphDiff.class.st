"
A PDPackageRelationGraphDiff do the diff between two PDPackageRelationGraph (an older and a newer) to detect :

 - which packages added/removed : packagesDiff.
 - which dependent packages added/removed from a package : dependentPackagesDiff.
 - which dependencies added/removed from a dependent package.
"
Class {
	#name : #DAPackageRelationGraphDiff,
	#superclass : #Object,
	#instVars : [
		'oldRelationGraph',
		'newRelationGraph',
		'packagesDiff',
		'dependentPackagesDiff'
	],
	#category : #'Tool-DependencyAnalyser-Diff'
}

{ #category : #private }
DAPackageRelationGraphDiff class >> loadFrom: aSerializeFile [ 
	^ self new
		oldRelationGraph: (FLMaterializer materializationFromFileNamed: aSerializeFile).
]

{ #category : #private }
DAPackageRelationGraphDiff class >> loadFrom: aSerializeFile with: newRelationGraph [
	^ self new
		oldRelationGraph: (FLMaterializer materializationFromFileNamed: aSerializeFile);
		newRelationGraph: newRelationGraph
]

{ #category : #adding }
DAPackageRelationGraphDiff >> addNewDependentPackage: package from: parent [
	^ dependentPackagesDiff
		add:
			((DADependentPackageItemDiff
				newWith: package)
				beNew;
				parent: parent;
				diff: self);
		yourself
]

{ #category : #adding }
DAPackageRelationGraphDiff >> addNewPackage: seenPackage [
	^ packagesDiff
		add:
			((DAPackageItemDiff newWith: seenPackage)
				beNew;
				diff: self);
		yourself
]

{ #category : #adding }
DAPackageRelationGraphDiff >> addRemovedDependentPackage: package from: parent [
	^ dependentPackagesDiff
		add:
			((DADependentPackageItemDiff
				newWith: package)
				beRemoved;
				parent: parent;
				diff: self);
		yourself
]

{ #category : #adding }
DAPackageRelationGraphDiff >> addRemovedPackage: seenPackage [
	^ packagesDiff
		add:
			((DAPackageItemDiff newWith: seenPackage)
				beRemoved;
				diff: self);
		yourself
]

{ #category : #adding }
DAPackageRelationGraphDiff >> addUndeterminateDependentPackage: aPackage from: parent [
	^ dependentPackagesDiff
		add:
				((DADependentPackageItemDiff newWith: aPackage)
						beUnderteminate;
						parent: parent;
						diff: self);
		yourself
]

{ #category : #adding }
DAPackageRelationGraphDiff >> addUndeterminatePackage: aPackage [
	^ packagesDiff
		add:
			((DAPackageItemDiff newWith: aPackage)
				beUnderteminate;
				diff: self);
				yourself
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> dependentPackagesDiff [
	^ dependentPackagesDiff
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> dependentPackagesDiffNew [
	^ dependentPackagesDiff select: [ :each | each isNew ]
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> dependentPackagesDiffRemoved [
	^ dependentPackagesDiff select: [ :each | each isRemoved ]
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> dependentPackagesDiffUnderteminate [
	^ dependentPackagesDiff select: [ :each | each isUnderteminate ]
]

{ #category : #initialization }
DAPackageRelationGraphDiff >> initialize [
	packagesDiff := OrderedCollection new.
	dependentPackagesDiff := OrderedCollection new.
]

{ #category : #searching }
DAPackageRelationGraphDiff >> lookUpForNewDependentPackages: newDependentPackages with: oldDependentPackages from: aPackage [
	
	| newDependent |
	newDependent := newDependentPackages reject: [ :package | oldDependentPackages includes: package ].
	newDependent do: [ :each | self addNewDependentPackage: each from: aPackage ].
]

{ #category : #searching }
DAPackageRelationGraphDiff >> lookUpForNewPackages [
			
	| newPackages |
	newPackages := newRelationGraph seenPackages reject: [ :package | oldRelationGraph includesSeenPackage: package ].
	newPackages do: [ :each | self addNewPackage: each ].
]

{ #category : #searching }
DAPackageRelationGraphDiff >> lookUpForRemovedDependentPackages: oldDependentPackages with: newDependentPackages from: aPackage [

	| removed |
	removed := oldDependentPackages reject: [ :package | newDependentPackages includes: package ].
	removed do: [ :each | self addRemovedDependentPackage: each from: aPackage ].
]

{ #category : #searching }
DAPackageRelationGraphDiff >> lookUpForRemovedPackages [
	| old |
	old := oldRelationGraph seenPackages reject: [ :each |  newRelationGraph includesSeenPackage: each ].	
	old do: [ :each |
		self addRemovedPackage: each
		 ]
]

{ #category : #searching }
DAPackageRelationGraphDiff >> lookUpForUndeterminateDependentPackagesFrom: parent [
	| oldDependentPackages undeterminates |
	oldDependentPackages := (oldRelationGraph packageAt: parent packageName) dependentPackages.
	undeterminates := oldDependentPackages reject: [ :package | self removedDependentPackagesIncludes: package from: parent ].
	
	
	undeterminates do: [ :each | self addUndeterminateDependentPackage: each from: parent ].

]

{ #category : #searching }
DAPackageRelationGraphDiff >> lookUpForUndeterminatePackages [
	| undeterminates |
	undeterminates := oldRelationGraph seenPackages reject: [ :package | self removedPackagesIncludes: package ].
	undeterminates do: [ :each | self addUndeterminatePackage: each ]
]

{ #category : #searching }
DAPackageRelationGraphDiff >> make [
	self 
		makePackagesDiff;	
		makeDependentPackagesDiff
		
]

{ #category : #searching }
DAPackageRelationGraphDiff >> makeDependenciesDiff [
]

{ #category : #searching }
DAPackageRelationGraphDiff >> makeDependentPackagesDiff [
	"we're just looking at the package undeterminate"
	"every dependent packages are removed or new in older or new packages"
	self packagesDiffUnderteminate do: [ :item |
			self makeDependentPackagesDiffOn: item content.
		 ]
	
]

{ #category : #searching }
DAPackageRelationGraphDiff >> makeDependentPackagesDiffOn: aPackage [
	
	| oldDependentPackages newDependentPackages  |
	
	oldDependentPackages := (oldRelationGraph packageAt: aPackage packageName) dependentPackages.
	newDependentPackages := (newRelationGraph packageAt: aPackage packageName) dependentPackages.
	
	self lookUpForRemovedDependentPackages: oldDependentPackages with: newDependentPackages from: aPackage.
	self lookUpForNewDependentPackages: newDependentPackages with: oldDependentPackages from: aPackage.
	self lookUpForUndeterminateDependentPackagesFrom: aPackage.
]

{ #category : #searching }
DAPackageRelationGraphDiff >> makePackagesDiff [
	"lookUp for new and remove packages"

	self
		lookUpForRemovedPackages;
		lookUpForNewPackages;
		 lookUpForUndeterminatePackages.
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> newRelationGraph [
	^ newRelationGraph
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> newRelationGraph: anObject [
	newRelationGraph := anObject
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> oldRelationGraph [
	^ oldRelationGraph
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> oldRelationGraph: anObject [
	oldRelationGraph := anObject
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> packageDiffNamed: aName [
	^ self packagesDiff detect: [ :item | item content packageName = aName ]
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> packagesDiff [
	^ packagesDiff
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> packagesDiffNew [
	^ packagesDiff select: [ :each | each isNew ]
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> packagesDiffNewSize [
	^ self packagesDiffNew size
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> packagesDiffRemoved [
	^ packagesDiff select: [ :each | each isRemoved ]
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> packagesDiffRemovedSize [
	^ self packagesDiffRemoved size
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> packagesDiffToDisplay [
	^ packagesDiff select: [ :each | each canBeDisplay ]
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> packagesDiffUnderteminate [
	^ packagesDiff select: [ :each | each isUnderteminate  ]
]

{ #category : #accessing }
DAPackageRelationGraphDiff >> packagesDiffUnderteminateSize [
	^ self packagesDiffUnderteminate size
]

{ #category : #testing }
DAPackageRelationGraphDiff >> removedDependentPackagesIncludes: aPackage from: parent [
	^ self dependentPackagesDiffRemoved
		detect: [ :package | package content = aPackage and: [ package parent = parent ] ]
		ifFound: [ true ]
		ifNone: [ false ]
]

{ #category : #testing }
DAPackageRelationGraphDiff >> removedPackagesIncludes: seenPackage [

	^ self packagesDiffRemoved detect: [ :each | each content = seenPackage ]
			ifFound: [ true ]
			ifNone: [ false ]

]
