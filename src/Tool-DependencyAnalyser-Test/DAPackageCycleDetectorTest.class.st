Class {
	#name : #DAPackageCycleDetectorTest,
	#superclass : #TestCase,
	#instVars : [
		'packageA',
		'packageB',
		'packageC',
		'packageD',
		'packageE',
		'aPackageCycleDetection',
		'cycleA',
		'cycleB',
		'cycleC'
	],
	#category : #'Tool-DependencyAnalyser-Test-Packages'
}

{ #category : #utilities }
DAPackageCycleDetectorTest >> cyclesSize [
	^ aPackageCycleDetection cycles size
]

{ #category : #utilities }
DAPackageCycleDetectorTest >> graphWithThreeCycles [
	| anArray |
	anArray := Array new: 3.
	anArray at: 1 put: packageA.
	anArray at: 2 put: packageB.
	anArray at: 3 put: packageC.
	packageA add: (DAPackageDependency from: packageA to: packageB).
	packageB add: (DAPackageDependency from: packageB to: packageA).
	packageA add: (DAPackageDependency from: packageA to: packageC).
	packageC add: (DAPackageDependency from: packageB to: packageA).
	packageC add: (DAPackageDependency from: packageB to: packageB).
	^ DAPackageRelationGraph onPackages: anArray
]

{ #category : #utilities }
DAPackageCycleDetectorTest >> queue [
	^ aPackageCycleDetection instVarNamed: 'queue'
]

{ #category : #running }
DAPackageCycleDetectorTest >> setUp [
	super setUp.
	aPackageCycleDetection := DAPackageCycleDetector new.
	packageA := DAPackage on: (RPackageSet named: 'A').
	packageB := DAPackage on: (RPackageSet named: 'B').
	packageC := DAPackage on: (RPackageSet named: 'C').
	packageD := DAPackage on: (RPackageSet named: 'D').
	packageE := DAPackage on: (RPackageSet named: 'E').
	cycleA := DAPackageCycle new.
	cycleB := DAPackageCycle new.
	cycleC := DAPackageCycle new.
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testAddCycle [
	aPackageCycleDetection addCycle: (DAPackageCycle new).
	self assert: (aPackageCycleDetection cycles size = 1)
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testAddVisitedNodes [
	aPackageCycleDetection initializeVisitedNodes.
	aPackageCycleDetection addVisitedNodes: packageA.
	self assert: (aPackageCycleDetection visitedNodes size = 1).
	self assert: (aPackageCycleDetection visitedNodes includes: packageA)
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testBuildCycleUntil [
	| cycle |
	packageA bfsParent: packageB.
	packageB bfsParent: packageC.
	packageC bfsParent: packageD.
	aPackageCycleDetection buildCycleUntil: packageA.
	self assert: self cyclesSize = 1.
	aPackageCycleDetection buildCycleUntil: packageA.
	"cycle already present"
	self assert: self cyclesSize = 1.
	cycle := aPackageCycleDetection  cycles first.
	self assert: (cycle isInCycle: packageA).
	self assert: (cycle  isInCycle: packageB).
	self assert: (cycle  isInCycle: packageC).
	self assert: (cycle  isInCycle: packageD).
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testCycleAlreadyFound [
	| cycle otherCycle |
	cycle := DAPackageCycle new.
	cycle addPackage: packageA.
	cycle addPackage: packageB.
	aPackageCycleDetection addCycle: cycle.
	otherCycle := DAPackageCycle new.
	otherCycle addPackage: packageA.
	self deny: (aPackageCycleDetection cycleAlreadyFound: otherCycle).
	otherCycle addPackage: packageB.
	self assert: (aPackageCycleDetection cycleAlreadyFound: otherCycle).
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testCyclesWith [
	cycleA addPackage: packageA;
			addPackage: packageB.
	
	cycleB addPackage: packageC;
			addPackage: packageD;
			addPackage: packageE.
	
	cycleC addPackage: packageA;
			addPackage: packageE.
	
	aPackageCycleDetection addCycle: cycleA;
		addCycle: cycleB;
		addCycle: cycleC.
		
	self assert: ((aPackageCycleDetection cyclesWith: packageA )size = 2).
	self assert: ((aPackageCycleDetection cyclesWith: packageC )size = 1).
	self assert: ((aPackageCycleDetection cyclesWith: packageE)size = 2).
	
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testCyclesWithAtLeast [
	| aCollectionOfPDPackage |
	aCollectionOfPDPackage  := OrderedCollection new.
	aCollectionOfPDPackage 
		add: packageA;
		add: packageB.

	cycleA addPackage: packageA;
			addPackage: packageB.
	
	cycleB addPackage: packageC;
			addPackage: packageD;
			addPackage: packageE.
	
	cycleC addPackage: packageE.
	
	aPackageCycleDetection addCycle: cycleA;
		addCycle: cycleB;
		addCycle: cycleC.
		
	self assert: ((aPackageCycleDetection cyclesWithAtLeast: aCollectionOfPDPackage )size = 1).
	
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testDequeue [
	| dequeue |
	aPackageCycleDetection initializeQueueWith: packageA.
	dequeue := aPackageCycleDetection dequeue.
	self assert: (dequeue = packageA).
	self assert: self queue isEmpty
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testEnqueue [
	aPackageCycleDetection initializeQueueWith: packageA.
		self assert: (self queue size = 1).
	aPackageCycleDetection enqueue: packageB.
	self assert: (self queue size = 2)
	
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testFindCyclesOne [
	"we must find 1 cycles in this acyclic graph"
	
	| aPDPackageRelation anArray |
	anArray := Array new: 3.
	anArray at: 1 put: packageA.
	anArray at: 2 put: packageB.
	anArray at: 3 put: packageC.
	(packageA) add: (DAPackageDependency from: (packageA) to: (packageB)).
	(packageB) add: (DAPackageDependency from: (packageB) to: (packageC)).
	(packageC) add: (DAPackageDependency from: (packageC) to: (packageA)).
	aPDPackageRelation := DAPackageRelationGraph onPackages: anArray.
	aPackageCycleDetection findCycles: aPDPackageRelation.
	self assert: (aPackageCycleDetection  cycles size = 1).
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testFindCyclesThreeNodes [
	| aPDPackageRelation |
	aPDPackageRelation := self graphWithThreeCycles.
	aPackageCycleDetection findCycles: aPDPackageRelation.
	self assert: aPackageCycleDetection cycles size = 3
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testInitializeQueueWith [	
	| dequeue |
	aPackageCycleDetection initializeQueueWith: packageA.
	self assert: self queue isNotNil.
	self assert: (self queue size = 1).
	dequeue := aPackageCycleDetection  dequeue.
	self assert: (dequeue = packageA).
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testIsVisitedOrInStack [
	aPackageCycleDetection initializeVisitedNodes.
	aPackageCycleDetection initializeQueueWith: packageA.
	self assert: (aPackageCycleDetection isVisitedOrInQueue: packageA).
	aPackageCycleDetection dequeue.
	self deny: (aPackageCycleDetection isVisitedOrInQueue: packageA).
	aPackageCycleDetection addVisitedNodes: packageA.
	self assert: (aPackageCycleDetection isVisitedOrInQueue: packageA)
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testReset [
	| aPDPackageRelation |
	aPDPackageRelation := self graphWithThreeCycles.
	aPackageCycleDetection findCycles: aPDPackageRelation.
	self assert: aPackageCycleDetection cycles size = 3.
	aPackageCycleDetection reset.
	self assert: aPackageCycleDetection cycles isEmpty
]

{ #category : #tests }
DAPackageCycleDetectorTest >> testfindCycles [
	"the graph test is the SCC 1 from the paper"
	| aPDPackageRelation anArray |
	anArray := Array new: 5.
	anArray at: 1 put: packageA.
	anArray at: 2 put: packageB.
	anArray at: 3 put: packageC.
	anArray at: 4 put: packageD.
	anArray at: 5 put: packageE.
	(packageA) add: (DAPackageDependency from: (packageA) to: (packageB)).
	(packageB) add: (DAPackageDependency from: (packageB) to: (packageC)).
	(packageB) add: (DAPackageDependency from: (packageB) to: (packageA)).
	(packageB) add: (DAPackageDependency from: (packageB) to: (packageD)).
	(packageC) add: (DAPackageDependency from: (packageC) to: (packageD)).
	(packageD) add: (DAPackageDependency from: (packageD) to: (packageE)).
	(packageD) add: (DAPackageDependency from: (packageD) to: (packageB)).
	(packageE) add: (DAPackageDependency from: (packageE) to: (packageA)).
	aPDPackageRelation := DAPackageRelationGraph onPackages: anArray.
	aPackageCycleDetection findCycles: aPDPackageRelation.
	self assert: (aPackageCycleDetection  cycles anySatisfy: [ :cycle | ((cycle size = 2) or: (cycle size= 4)) or: (cycle size = 3) ]).
	
]

{ #category : #utilities }
DAPackageCycleDetectorTest >> visitedNodes [
	^ aPackageCycleDetection instVarNamed: 'visitedNodes'
]
