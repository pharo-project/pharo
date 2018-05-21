Class {
	#name : #DAPackageRelationGraphTest,
	#superclass : #TestCase,
	#instVars : [
		'packageCollectionAbstract',
		'packageCollectionsSequenceable',
		'packageKernel',
		'packageRegexCore',
		'packageStrings',
		'aPackageRelationGraph',
		'packagePackageDependencies',
		'packageA',
		'packageB',
		'packageC',
		'packageD'
	],
	#category : #'Tool-DependencyAnalyser-Test-Packages'
}

{ #category : #utilities }
DAPackageRelationGraphTest >> packageDependencyFrom: aPackage to: anOtherPackage [
	^ DAPackageDependency from: aPackage to: anOtherPackage
]

{ #category : #running }
DAPackageRelationGraphTest >> setUp [
	super setUp.
	aPackageRelationGraph := DAPackageRelationGraph new.
	packageCollectionAbstract := DAPackage on: (RPackageSet named: 'Collections-Abstract').
	packageStrings := DAPackage on:(RPackageSet named: 'Collections-Strings').
	packageKernel := DAPackage on:(RPackageSet named: 'Kernel').
	packageRegexCore := DAPackage on:(RPackageSet named: 'Regex-Core').
	packageCollectionsSequenceable := DAPackage on: (RPackageSet named: 'Collections-Sequenceable').
	packagePackageDependencies := DAPackage on: (RPackageSet named:  #'Tool-DependencyAnalyser').
	packageA := DAPackage on: (RPackageSet named: 'A').
	packageB := DAPackage on: (RPackageSet named: 'B').
	packageC := DAPackage on: (RPackageSet named: 'C').
	packageD := DAPackage on: (RPackageSet named: 'D').
]

{ #category : #tests }
DAPackageRelationGraphTest >> testAddExtensionDependencies [
	aPackageRelationGraph addExtensionDependencies: packageCollectionAbstract.
	"self assert: (packageStrings  dependentPackages includes: packageCollectionAbstract)."
	"self assert: (packageCollectionAbstract dependentPackages includes: packageKernel).
	self assert: (packageCollectionAbstract dependentPackages includes: packageRegexCore).
	self assert: (packageCollectionAbstract dependentPackages includes: packageCollectionsSequenceable)."

	

]

{ #category : #tests }
DAPackageRelationGraphTest >> testAddInheritanceDependencies [
	aPackageRelationGraph addInheritanceDependencies: packageCollectionAbstract.
	"Collections-Abstract has one dependency with Error and Object by Inheritance"
	self assert: (packageCollectionAbstract dependentPackages includes: packageKernel).
]

{ #category : #tests }
DAPackageRelationGraphTest >> testAddPackage [
	self assert: aPackageRelationGraph packages isEmpty.
	aPackageRelationGraph addPackage: packageCollectionAbstract.
	self assert: aPackageRelationGraph packages size = 1.
	aPackageRelationGraph addPackage: packageCollectionsSequenceable.
	self assert: aPackageRelationGraph packages size = 2.
]

{ #category : #tests }
DAPackageRelationGraphTest >> testAddPackages [
	| array |
	self assert: aPackageRelationGraph packages isEmpty.
	array := Array with: packageCollectionAbstract with: packageCollectionsSequenceable with: packageKernel.
	aPackageRelationGraph addPackages: array.
	self assert: aPackageRelationGraph packages size = 3
]

{ #category : #tests }
DAPackageRelationGraphTest >> testAddReferenceDependencies [
	aPackageRelationGraph addReferenceDependencies: packagePackageDependencies.
	packagePackageDependencies removeAllInternal.
	self assert: (packagePackageDependencies  dependentPackages includes: packageStrings).
]

{ #category : #tests }
DAPackageRelationGraphTest >> testAllDependentPackages [
	
		packageA add: (DAPackageDependency from: packageA to: packageB).

]

{ #category : #tests }
DAPackageRelationGraphTest >> testAllDependentPackagesFromSeenPackages [
	packageA
		add: (self packageDependencyFrom: packageA to: packageB);
		add: (self packageDependencyFrom: packageA to: packageC).
	packageB add: (self packageDependencyFrom: packageB to: packageC).
	packageB add: (self packageDependencyFrom: packageB to: packageA).
	
	packageA beSeen.
	packageB beSeen.
	aPackageRelationGraph addPackage: packageA;
							addPackage: packageB.
	self assert: (aPackageRelationGraph allDependentPackagesFromSeenPackages size = 3).
]

{ #category : #tests }
DAPackageRelationGraphTest >> testAllSeen [
	aPackageRelationGraph addPackage: packageKernel.
	self deny: (aPackageRelationGraph allSeen).
	aPackageRelationGraph computeStaticDependencies.
	self deny: (aPackageRelationGraph allSeen)
]

{ #category : #tests }
DAPackageRelationGraphTest >> testClearAllDependencies [
	| aRelationGraph packagesFromGraph |
	aRelationGraph := DAPackageRelationGraph
		onPackages:
			(#('Collections-Abstract' 'Collections-Sequenceable') collect: [ :each | DAPackage on: (RPackageSet named: each) ]).
	aRelationGraph
		build;
		clearAllDependencies.
	packagesFromGraph := aRelationGraph seenPackages.
	self assert: (packagesFromGraph anySatisfy: [ :package | package dependencies isEmpty ])
]

{ #category : #tests }
DAPackageRelationGraphTest >> testCombineDependencies [
	| aSet |
	aSet := Set new.
	aPackageRelationGraph computeStaticDependencies:packageCollectionAbstract.
	packageCollectionAbstract removeAllInternal.
	"we collect all the target PDPackage"
	packageCollectionAbstract dependencies do: [ :dependency |
		aSet add: dependency target.
	  ].
	aPackageRelationGraph combineDependencies: packageCollectionAbstract.
	aSet do: [ :each | self assert: (packageCollectionAbstract dependentPackages includes: each) ]
	

	
	
	
	
	
]

{ #category : #tests }
DAPackageRelationGraphTest >> testComputeDependencies [
	aPackageRelationGraph computeStaticDependencies: packageCollectionAbstract.
	self deny: (packageCollectionAbstract dependencies isEmpty)
]

{ #category : #tests }
DAPackageRelationGraphTest >> testDependenciesFrom [
	| aCollection |
	aCollection := OrderedCollection new.
	aCollection add: packageA.
	aCollection add: packageB.
	aCollection add: packageC.
	(aCollection at:1) add: (DAPackageDependency from: packageA to: packageB).
	(aCollection at:1) add: (DAPackageDependency from:packageA to: packageC).
	(aCollection at:2) add: (DAPackageDependency from: packageB to: packageC).
	aCollection do: [ :package | aPackageRelationGraph addPackage: package ].
	self assert: ((aPackageRelationGraph dependenciesFrom:'A') size = 2).
	self assert: ((aPackageRelationGraph dependenciesFrom:'B') size = 1).
	self assert: ((aPackageRelationGraph dependenciesFrom:'C') isEmpty).
]

{ #category : #tests }
DAPackageRelationGraphTest >> testIncludedPackages [
	| aCollection |
	aCollection := Bag new.
	"we add 2 packages in the relation"
	aCollection add: packagePackageDependencies .
	aCollection add: packageKernel.
	aCollection do: [ :each | aPackageRelationGraph addPackage: each].
	self assert: (aPackageRelationGraph includedPackages  size = 2).
]

{ #category : #tests }
DAPackageRelationGraphTest >> testPackageAt [

	aPackageRelationGraph addPackage: packageKernel.
	self assert: ( (aPackageRelationGraph packageAt: 'Kernel') isNotNil).
]

{ #category : #tests }
DAPackageRelationGraphTest >> testPackages [
	| aCollection |
	self assert: aPackageRelationGraph packages isEmpty.
	aCollection := Bag new.
	aCollection add: packagePackageDependencies.
	aCollection add: packageKernel.
	aCollection do: [ :each | aPackageRelationGraph addPackage: each].
	self assert: (aPackageRelationGraph packages size = 2)
]

{ #category : #tests }
DAPackageRelationGraphTest >> testPdPackageFrom [
	aPackageRelationGraph addPackage: packageCollectionAbstract.
	self assert: ((aPackageRelationGraph pdPackageFrom: Collection) = packageCollectionAbstract )
]

{ #category : #tests }
DAPackageRelationGraphTest >> testPredecessors [
	| aCollection |
	"A -> B, C, D ; B -> D, C"
	aCollection := OrderedCollection new.
	aCollection add: packageA.
	aCollection add: packageB.
	aCollection add: packageC.
	aCollection add: packageD.
	packageA add: (DAPackageDependency from:packageA to: packageB).
	packageA add: (DAPackageDependency from:packageA to: packageC).
	packageA add: (DAPackageDependency from:packageA to: packageD).
	packageB add: (DAPackageDependency from:packageB to: packageC).
	packageB add: (DAPackageDependency from:packageB to: packageD).
	aCollection do: [ :package | aPackageRelationGraph addPackage: package ].
	self assert: ((aPackageRelationGraph predecessors: packageC) size = 2).
	self assert: ((aPackageRelationGraph predecessors: (aCollection at: 1)) isEmpty).
	self assert: ((aPackageRelationGraph predecessors: packageB) size = 1).
	self assert: ((aPackageRelationGraph predecessors: packageD) size = 2).
]

{ #category : #tests }
DAPackageRelationGraphTest >> testRemoveInternalDependencies [
	aPackageRelationGraph computeStaticDependencies.
	aPackageRelationGraph removeInternalDependencies.
	
	aPackageRelationGraph packages do: [ :value |
				value dependencies do: 
					[ :dependency |
					self assert: (dependency isExternal).
					 ] ].
]

{ #category : #tests }
DAPackageRelationGraphTest >> testRemoveOutgoingDependencies [
	aPackageRelationGraph computeStaticDependencies.
	aPackageRelationGraph removeOutgoingDependencies.
	
	aPackageRelationGraph packages do: [ :package |
			package dependencies do: [ :dependency |
				self assert: (dependency target isIncluded)
				]
		 ].
]

{ #category : #tests }
DAPackageRelationGraphTest >> testRemovePackage [
	self assert: aPackageRelationGraph packages isEmpty.
	aPackageRelationGraph addPackage: packageCollectionAbstract.
	self assert: aPackageRelationGraph packages size = 1.
	aPackageRelationGraph removePackage: packageCollectionAbstract packageName.
	self assert: aPackageRelationGraph packages isEmpty
]

{ #category : #tests }
DAPackageRelationGraphTest >> testSeenPackages [
	aPackageRelationGraph addPackage: packageKernel.
	aPackageRelationGraph addPackage: packageCollectionAbstract.
	self assert: (aPackageRelationGraph seenPackages isEmpty).
	aPackageRelationGraph computeStaticDependencies.
	self assert: (aPackageRelationGraph seenPackages size = 2).
]

{ #category : #tests }
DAPackageRelationGraphTest >> testSeenPackagesName [
	aPackageRelationGraph addPackage: packageKernel.
	self deny: (aPackageRelationGraph seenPackagesName includes: 'Kernel').
	aPackageRelationGraph computeStaticDependencies.
	self assert: (aPackageRelationGraph seenPackagesName includes: 'Kernel').
]

{ #category : #tests }
DAPackageRelationGraphTest >> testSuccessors [
	| aCollection |
	"A -> B, C, D ; B -> D, C"
	aCollection := OrderedCollection new.
	aCollection add: packageA.
	aCollection add: packageB.
	aCollection add: packageC.
	aCollection add: packageD.
	packageA add: (DAPackageDependency from:packageA to: packageB).
	packageA add: (DAPackageDependency from:packageA to: packageC).
	packageA add: (DAPackageDependency from:packageA to: packageD).
	packageB add: (DAPackageDependency from:packageB to: packageC).
	packageB add: (DAPackageDependency from:packageB to: packageD).
	aCollection do: [ :package | aPackageRelationGraph addPackage: package ].
	self assert: ((aPackageRelationGraph successors: (aCollection at: 1)) size = 3).
	self assert: ((aPackageRelationGraph successors: (aCollection at: 4)) isEmpty).
	self assert: ((aPackageRelationGraph successors: (aCollection at: 2)) size = 2).
	self assert: ((aPackageRelationGraph successors: (aCollection at: 3)) isEmpty).
]

{ #category : #tests }
DAPackageRelationGraphTest >> testSystemPackageContaining [
	self assert: ( (aPackageRelationGraph systemPackageContaining: Collection) packageName = 'Collections-Abstract').
	self assert: ( (aPackageRelationGraph systemPackageContaining: DAPackageRelationGraph) packageName =  #'Tool-DependencyAnalyser')
]

{ #category : #tests }
DAPackageRelationGraphTest >> testUsersOf [
	packageA 
		add: (self packageDependencyFrom: packageA to:  packageB);
		add: (self packageDependencyFrom: packageA to:  packageC);
		beSeen.
	packageB
		add: (self packageDependencyFrom: packageA to:  packageC);
		beSeen.
		
	aPackageRelationGraph
		 addPackage: packageA;
		 addPackage: packageB.
		
	self assert: ((aPackageRelationGraph usersOf: packageC) size = 2).
	self assert: ((aPackageRelationGraph usersOf: packageC) includes: packageB).
			
]
