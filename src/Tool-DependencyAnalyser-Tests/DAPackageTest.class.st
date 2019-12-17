Class {
	#name : #DAPackageTest,
	#superclass : #TestCase,
	#instVars : [
		'aPackage',
		'aSecondPackage',
		'packageA',
		'packageB',
		'packageC'
	],
	#category : #'Tool-DependencyAnalyser-Tests-Packages'
}

{ #category : #running }
DAPackageTest >> setUp [
	super setUp.
	aPackage := DAPackage on: (RPackageSet named: 'Collections-Abstract'). 
	aSecondPackage := DAPackage on: (RPackageSet named: 'Collections-Arithmetic').
	packageA := DAPackage on: (RPackageSet named: 'A').
	packageB := DAPackage on: (RPackageSet named: 'B').
	packageC := DAPackage on: (RPackageSet named: 'C').
	aPackage add: (DAInheritanceDependency from:aPackage to:aSecondPackage).
]

{ #category : #tests }
DAPackageTest >> testAdd [
	| aDependency |
	aDependency := DAPackageDependency from: aPackage to: aSecondPackage.
	aPackage add: aDependency.

	self assert: aPackage dependencies size equals: 2.
	self assert: (aPackage dependencies includes: aDependency)
]

{ #category : #tests }
DAPackageTest >> testBeIncluded [
	aPackage beIncluded.
	self assert: (aPackage isIncluded)
]

{ #category : #tests }
DAPackageTest >> testCleardependencies [
	aPackage clearDependencies.
	self assertEmpty: aPackage dependencies
]

{ #category : #tests }
DAPackageTest >> testDependantPackages [
	| aCollection |
	aPackage add: (DAPackageDependency from: aPackage to: packageA).

	aCollection := aPackage dependentPackages.
	self assert: (aCollection includes: packageA).
	self assert: (aCollection includes: aSecondPackage).
	self assert: aCollection size equals: 2
]

{ #category : #tests }
DAPackageTest >> testDependenciesFrom [
	| aRelationGraph |
	aRelationGraph := DAPackageRelationGraph new.
	[ aRelationGraph
		computeStaticDependencies: aPackage;
		combineDependencies: aPackage ]
		on: PotentialOutDatedDependencyWarning
		do: [ :ex | ex resume "ignore" ].
	self
		assert:
			((aPackage dependenciesFrom: Collection) allSatisfy: [ :dependency | dependency theClass = Collection ])
]

{ #category : #tests }
DAPackageTest >> testDependenciesSize [
	| aCompositeDependency |
	aCompositeDependency := DACompositeDependency from: packageA to: packageB.
	aCompositeDependency add: (DAInheritanceDependency from: aPackage to: packageB).
	aCompositeDependency add: (DAInheritanceDependency from: aPackage to: packageB).
	self assert: aPackage dependenciesSize equals: 1.
	aPackage add: aCompositeDependency.
	self assert: aPackage dependenciesSize equals: 3
]

{ #category : #running }
DAPackageTest >> testDependenciesSizeTo [
	packageA
		add: (DAInheritanceDependency from: packageA to: packageB);
		add: (DAInheritanceDependency from: packageA to: packageC).
	self assert: (packageA dependenciesSizeTo: packageB) equals: 1
]

{ #category : #tests }
DAPackageTest >> testDependenciesTo [
	self assert: (aPackage dependenciesTo: aSecondPackage packageName) size equals: 1.
	self assertEmpty: (aPackage dependenciesTo: 'Test')
]

{ #category : #tests }
DAPackageTest >> testDependentPackages [
	self assert: aPackage dependentPackages size equals: 1.
	self assert: (aPackage dependentPackages anySatisfy: [ :package | package = aSecondPackage ]).
	self assertEmpty: aSecondPackage dependentPackages
]

{ #category : #tests }
DAPackageTest >> testDependentPackagesSize [
	| dependency |
	self assert: aPackage dependentPackagesSize equals: 1.
	dependency := DAPackageDependency from: aPackage to: packageA.
	aPackage add: dependency.
	self assert: aPackage dependentPackagesSize equals: 2
]

{ #category : #tests }
DAPackageTest >> testNumberOfDependentPackage [
	self assert: aPackage dependentPackagesSize equals: 1.
	aPackage add: (DAPackageDependency from: aPackage to: packageA).
	self assert: aPackage dependentPackagesSize equals: 2
]

{ #category : #tests }
DAPackageTest >> testPackageName [
	self assert: packageA packageName equals: 'A'
]

{ #category : #tests }
DAPackageTest >> testReferenceDependenciesFrom [
	| aRelationGraph |
	aRelationGraph := DAPackageRelationGraph new.
	[ aRelationGraph
		computeStaticDependencies: aPackage;
		combineDependencies: aPackage ]
	on: PotentialOutDatedDependencyWarning
	do: [ :ex | ex resume "ignore" ].
	self
		assert:
			((aPackage referenceDependenciesFrom: Collection) allSatisfy: [ :dependency | dependency isReferenceDependency ]).
	self
		assert:
			((aPackage referenceDependenciesFrom: Collection) allSatisfy: [ :dependency | dependency theClass = Collection ])
]

{ #category : #tests }
DAPackageTest >> testRemoveAllInternal [
	aPackage clearDependencies.
	aPackage add: (DAPackageDependency from: aPackage to: aPackage).
	aPackage add: (DAPackageDependency from: aSecondPackage to: aSecondPackage).
	aPackage removeAllInternal.
	self assertEmpty: aPackage dependencies
]
