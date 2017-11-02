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
	#category : 'Tool-DependencyAnalyser-Test'
}

{ #category : #running }
DAPackageTest >> setUp [
	aPackage := DAPackage on: (RPackageSet named: 'Collections-Abstract'). 
	aSecondPackage := DAPackage on: (RPackageSet named: 'Collections-Arithmetic').
	packageA := (DAPackage on: (RPackageSet named: 'A')).
	packageB := (DAPackage on: (RPackageSet named: 'B')).
	packageC := (DAPackage on: (RPackageSet named: 'C')).
	aPackage add: (DAInheritanceDependency from:aPackage to:aSecondPackage).
]

{ #category : #tests }
DAPackageTest >> testAdd [
	| aDependency |
	aDependency := DAPackageDependency from:aPackage to:aSecondPackage.
	aPackage add: aDependency.
	
	self assert: ( (aPackage dependencies size) = 2).
	self assert: ( aPackage dependencies includes: aDependency)
]

{ #category : #tests }
DAPackageTest >> testBeIncluded [
	aPackage beIncluded.
	self assert: (aPackage isIncluded)
]

{ #category : #tests }
DAPackageTest >> testCleardependencies [
	aPackage clearDependencies.
	self assert: (aPackage dependencies isEmpty)
]

{ #category : #tests }
DAPackageTest >> testDependantPackages [
	| aCollection |
	
	aPackage add: (DAPackageDependency from:aPackage to:packageA).
	
	aCollection := aPackage dependentPackages.
	self assert: (aCollection includes: packageA).
	self assert: (aCollection includes: aSecondPackage).
	self assert: (aCollection size = 2).
	
	
	
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
	aCompositeDependency := DACompositeDependency
		from: packageA
		to: packageB.
	aCompositeDependency
		add:
			(DAInheritanceDependency from:  aPackage to: packageB).
	aCompositeDependency
		add:
			(DAInheritanceDependency from:  aPackage to: packageB).
	self assert: (aPackage dependenciesSize = 1).
	aPackage add: aCompositeDependency.
	self assert: (aPackage dependenciesSize = 3).
	
]

{ #category : #running }
DAPackageTest >> testDependenciesSizeTo [
	packageA
		add: (DAInheritanceDependency from: packageA to: packageB);
		add: (DAInheritanceDependency from: packageA to: packageC).
	self assert: ((packageA dependenciesSizeTo: packageB )= 1).
]

{ #category : #tests }
DAPackageTest >> testDependenciesTo [
	self assert: (aPackage dependenciesTo: aSecondPackage packageName) size = 1.
	self assert: (aPackage dependenciesTo: 'Test') isEmpty
]

{ #category : #tests }
DAPackageTest >> testDependentPackages [
	self assert: (aPackage dependentPackages size = 1).
	self assert: (aPackage dependentPackages anySatisfy: [ :package | package = aSecondPackage ]).
	self assert: (aSecondPackage dependentPackages isEmpty)
]

{ #category : #tests }
DAPackageTest >> testDependentPackagesSize [
	| dependency |
	self assert: aPackage dependentPackagesSize = 1.
	dependency := DAPackageDependency from: aPackage to: packageA.
	aPackage add: dependency.
	self assert: aPackage dependentPackagesSize = 2
]

{ #category : #tests }
DAPackageTest >> testNumberOfDependentPackage [
	self assert: (aPackage dependentPackagesSize = 1).
	aPackage add: (DAPackageDependency from: aPackage to: packageA).
	self assert: (aPackage dependentPackagesSize = 2).
]

{ #category : #tests }
DAPackageTest >> testPackageName [
	self assert:  (packageA packageName = 'A')
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
	aPackage add: (DAPackageDependency from:aPackage to:aPackage).
	aPackage add: (DAPackageDependency from:aSecondPackage to:aSecondPackage).
	aPackage removeAllInternal.
	self assert: (aPackage dependencies isEmpty)
]
