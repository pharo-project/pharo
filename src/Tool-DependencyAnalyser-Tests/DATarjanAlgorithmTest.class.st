Class {
	#name : #DATarjanAlgorithmTest,
	#superclass : #TestCase,
	#instVars : [
		'tarjanPackage',
		'packageA',
		'packageB',
		'packageC',
		'packageD',
		'packageE',
		'packageF',
		'packageG',
		'packageH',
		'anArray'
	],
	#category : #'Tool-DependencyAnalyser-Tests-Algorithm'
}

{ #category : #running }
DATarjanAlgorithmTest >> setUp [
	super setUp.
	tarjanPackage := AITarjan new.
	anArray := Array new: 8.
	packageA := DAPackage on: (RPackageSet named: 'A').
	packageB := DAPackage on: (RPackageSet named: 'B').
	packageC := DAPackage on: (RPackageSet named: 'C').
	packageD := DAPackage on: (RPackageSet named: 'D').
	packageE := DAPackage on: (RPackageSet named: 'E').
	packageF := DAPackage on: (RPackageSet named: 'F').
	packageG := DAPackage on: (RPackageSet named: 'G').
	packageH := DAPackage on: (RPackageSet named: 'H').
	anArray at: 1 put: packageA.
	anArray at: 2 put: packageB.
	anArray at: 3 put: packageC.
	anArray at: 4 put: packageD.
	anArray at: 5 put: packageE.
	anArray at: 6 put: packageF.
	anArray at: 7 put: packageG.
	anArray at: 8 put:packageH
]

{ #category : #utilities }
DATarjanAlgorithmTest >> stackSize [
	^ (tarjanPackage instVarNamed: 'stack') size
]

{ #category : #tests }
DATarjanAlgorithmTest >> testNoOutgoingDependenciesAfterTarjan [
	"test if we have 0 outgoing dependencies in each SCC after the algorithm"

	| aRelation aCollection sccs |
	aCollection := OrderedCollection withAll: (Smalltalk organization packages
			                select: [ :package | '*Collections*' match: package packageName asString ]
			                thenCollect: [ :package | package packageName ]).
	aRelation := DAPackageRelationGraph onPackages: (aCollection collect: [ :each | DAPackage on: (RPackageSet named: each) ]).
	aRelation
		computeStaticDependencies;
		removeInternalDependencies;
		removeOutgoingDependencies;
		combineDependencies.

	sccs := tarjanPackage runOnDAPackageRelationGraph: aRelation.

	self assert: (sccs anySatisfy: [ :scc | scc outgoing isEmpty ])
]

{ #category : #tests }
DATarjanAlgorithmTest >> testRunTarjan [
	"test the Tarjan algorithm on a simple graph with 8 nodes"
	| aPDPackageRelation  sccs |
	packageA add: (DAPackageDependency from: packageA to: packageB).
	packageB add: (DAPackageDependency from: packageB to: packageC).
	packageC add: (DAPackageDependency from: packageC to: packageD).
	packageD add: (DAPackageDependency from: packageD to: packageE).
	packageE add: (DAPackageDependency from: packageE to: packageA).
	packageC add: (DAPackageDependency from: packageC to: packageF).
	packageF add: (DAPackageDependency from: packageF to: packageG).
	packageG add: (DAPackageDependency from: packageG to: packageH).
	packageH add: (DAPackageDependency from: packageH to: packageG).
	aPDPackageRelation := DAPackageRelationGraph onPackages: anArray.
	sccs := tarjanPackage runOnDAPackageRelationGraph: aPDPackageRelation.

	"we must find 3 sccs, five two and one nodes"
	self assert: sccs size equals: 3.
	self assert: (sccs anySatisfy: [ :each | each packages size = 5 ]).
	self assert: (sccs anySatisfy: [ :each | each packages size = 2 ]).
	self assert: (sccs anySatisfy: [ :each | each packages size = 1 ])
]
