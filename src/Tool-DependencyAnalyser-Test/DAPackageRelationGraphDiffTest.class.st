Class {
	#name : #DAPackageRelationGraphDiffTest,
	#superclass : #TestCase,
	#instVars : [
		'oldRelationGraph',
		'newRelationGraph',
		'packageRelationGraphDiff',
		'packageA',
		'packageB'
	],
	#category : #'Tool-DependencyAnalyser-Test'
}

{ #category : #running }
DAPackageRelationGraphDiffTest >> setUp [
	| packageCollectionsAbstract packageCollectionsString |
	packageA := DAPackage on: (RPackageSet named: 'A').
	packageB := DAPackage on: (RPackageSet named: 'B').
	packageCollectionsAbstract := DAPackage on: (RPackageSet named: 'Collections-Abstract').
	packageCollectionsString := DAPackage on: (RPackageSet named: 'Collections-Strings').
	oldRelationGraph := DAPackageRelationGraph onPackages: (Array with: packageCollectionsAbstract with: packageCollectionsString ).
	newRelationGraph := DAPackageRelationGraph
		onPackages: (Array with: (DAPackage on: (RPackageSet named: 'Collections-Abstract')) with: (DAPackage on: (RPackageSet named: 'Collections-Strings'))).
	oldRelationGraph build.
	newRelationGraph build.
	

	packageRelationGraphDiff := DAPackageRelationGraphDiff new
		oldRelationGraph: oldRelationGraph;
		newRelationGraph: newRelationGraph
]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testHasChangedForPackageItemDiff [
	| first second |
	first := newRelationGraph seenPackages first.
	second := newRelationGraph seenPackages last.
	
	first clearDependencies.
	
	packageRelationGraphDiff make.
			
	self assert: ((packageRelationGraphDiff packageDiffNamed: first packageName) hasChanged).
	self deny: ((packageRelationGraphDiff packageDiffNamed: second packageName) hasChanged).

]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testHasChangedForPackageItemDiffOnOneDependentPackageRemoved [
	| first |
	first := newRelationGraph seenPackages first.
	first dependencies remove: (first dependencies anyOne).
	
	packageRelationGraphDiff make.
			
	self assert: ((packageRelationGraphDiff packageDiffNamed: first packageName) hasChanged).
	
]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testMakeDependentPackagesDiffOnSameGraph [
	packageRelationGraphDiff makePackagesDiff.
		
	self assert: packageRelationGraphDiff dependentPackagesDiff isEmpty
	
]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testMakeDependentPackagesDifferentOnDifferentGraph [
	| oldSize |
	oldSize := newRelationGraph seenPackages first dependentPackages size.
	newRelationGraph seenPackages first clearDependencies.

	packageRelationGraphDiff make.
		
	self assert: (packageRelationGraphDiff dependentPackagesDiffRemoved size = oldSize).
	self assert: (packageRelationGraphDiff dependentPackagesDiffUnderteminate size = (newRelationGraph seenPackages last dependentPackages size))
	
]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testMakePackagesDiffOnDifferentGraph [
	newRelationGraph removePackage: 'Collections-Abstract'.
	packageRelationGraphDiff makePackagesDiff.
	
	self assert: (packageRelationGraphDiff packagesDiffRemovedSize = 1).
	self assert: (packageRelationGraphDiff packagesDiffUnderteminateSize = 1).
]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testMakePackagesDiffOnSameGraph [
	packageRelationGraphDiff makePackagesDiff.
	
	self assert: (packageRelationGraphDiff packagesDiff size = oldRelationGraph seenPackages size).
	self assert: (packageRelationGraphDiff packagesDiff allSatisfy: [ :item | item isUnderteminate  ]).
]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testPackagesDiffToDisplay [
	| first second |
	first := newRelationGraph seenPackages first.
	second := newRelationGraph seenPackages last.
	
	first clearDependencies.
	second dependencies remove: (second dependencies anyOne).
	
	packageRelationGraphDiff make.
	self assert: (packageRelationGraphDiff packagesDiffToDisplay size = 2).

]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testPackagesDiffToDisplayBis [
	| first  |
	first := newRelationGraph seenPackages first.
	first clearDependencies.
	packageRelationGraphDiff make.
		
	self assert: (packageRelationGraphDiff packagesDiffToDisplay size = 1).

]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testPackagesDiffToDisplayOnSameGraph [
	
	packageRelationGraphDiff make.
			
	self assert: (packageRelationGraphDiff packagesDiffToDisplay isEmpty).

]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testRemovedDependentPackagesIncludesFrom [
	| dependentPackages package |
	package := newRelationGraph seenPackages first.
	dependentPackages := package dependentPackages.
	
	package clearDependencies.
	
	packageRelationGraphDiff make.
			
	self assert: (packageRelationGraphDiff removedDependentPackagesIncludes: dependentPackages anyOne from: package).
			
	
	

]

{ #category : #running }
DAPackageRelationGraphDiffTest >> testRemovedPackagesIncludes [
	newRelationGraph clearPackages .
	packageRelationGraphDiff makePackagesDiff.
	
	self assert: (packageRelationGraphDiff removedPackagesIncludes: (DAPackage on: (RPackageSet named: 'Collections-Abstract'))).
	self assert: (packageRelationGraphDiff removedPackagesIncludes: (DAPackage on: (RPackageSet named: 'Collections-Strings'))).
	
]
