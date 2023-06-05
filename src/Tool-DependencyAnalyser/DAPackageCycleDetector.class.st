"
I'm able to detect all the cycles in a package dependency graph.

Analysis is done in three steps: build the dependency graph, isolating the strongly connected components (SCC), and for each SCC detect all the elementary cycles.

Use the message runAlgorithm to run the algorithm to retrieve the elementary cycles in the package dependency graph.

 At the end, all the cycles are in the collection ""cycles"".
"
Class {
	#name : #DAPackageCycleDetector,
	#superclass : #Object,
	#instVars : [
		'sccs',
		'cycles',
		'visitedNodes',
		'queue',
		'relationGraph'
	],
	#category : #'Tool-DependencyAnalyser-Core'
}

{ #category : #'instance creation' }
DAPackageCycleDetector class >> collections [
	^ (self onPackagesNamed: #('Collections-Abstract' 'Collections-Arithmetic' 'Collections-Arrayed' 'Collections-Atomic' 'Collections-Native' 'Collections-Sequenceable' 'Collections-Stack' 'Collections-Streams' 'Collections-Strings' 'Collections-Support' 'Collections-Traits' 'Collections-Unordered' 'Collections-Weak')) runAlgorithm
]

{ #category : #'instance creation' }
DAPackageCycleDetector class >> morphic [
	^ (self onPackagesNamed: #('Morphic-Base' 'Morphic-Core' 'Morphic-Examples' 'MorphicTests' 'Spec-MorphicAdapters')) runAlgorithm
]

{ #category : #'instance creation' }
DAPackageCycleDetector class >> new [
	^ self onPackagesNamed: #()
]

{ #category : #'instance creation' }
DAPackageCycleDetector class >> onPackages: aCollection [
	^ self basicNew
		initializeWithRPackageSet: aCollection;
		yourself
]

{ #category : #'instance creation' }
DAPackageCycleDetector class >> onPackagesNamed: aCollection [
	^ self onPackages: (aCollection collect: [ :each | RPackageSet named: each ])
]

{ #category : #'instance creation' }
DAPackageCycleDetector class >> system [
	^ (self onPackagesNamed:
		(Smalltalk organization packages collect: [ :package | package packageName asString ])) runAlgorithm
]

{ #category : #adding }
DAPackageCycleDetector >> addCycle: aPDPackageCycle [
	cycles add: aPDPackageCycle
]

{ #category : #adding }
DAPackageCycleDetector >> addVisitedNodes: aNode [
	visitedNodes add: aNode
]

{ #category : #actions }
DAPackageCycleDetector >> buildCycleUntil: children [
	| aPackageCycle currentNode |
	aPackageCycle := DAPackageCycle new.
	currentNode := children.
	[ currentNode isNil ]
		whileFalse: [
			aPackageCycle addPackage: currentNode.
			currentNode := currentNode bfsParent ].
	(self cycleAlreadyFound: aPackageCycle)
		ifFalse: [
			aPackageCycle reversedCycle.
			self addCycle: aPackageCycle ]
]

{ #category : #accessing }
DAPackageCycleDetector >> childrensFrom: stronglyConnectedComponents for: aNode [
	^ stronglyConnectedComponents successors: aNode
]

{ #category : #testing }
DAPackageCycleDetector >> cycleAlreadyFound: aPackageCycle [
	^ cycles anySatisfy: [ :cycle | cycle = aPackageCycle ]
]

{ #category : #accessing }
DAPackageCycleDetector >> cycles [
	^ cycles sorted: [ :a :b | a size < b size ]
]

{ #category : #accessing }
DAPackageCycleDetector >> cycles: anObject [
	cycles := anObject
]

{ #category : #accessing }
DAPackageCycleDetector >> cyclesWith: aPDPackage [
	^ cycles select: [ :cycle |
			cycle isInCycle: aPDPackage
		 ]
]

{ #category : #accessing }
DAPackageCycleDetector >> cyclesWithAtLeast: aCollectionOfPDPackage [
	^ cycles select: [ :cycle | cycle isAtLeastInCycle: aCollectionOfPDPackage ]
]

{ #category : #actions }
DAPackageCycleDetector >> dequeue [
	| aNode |
	aNode := queue nextOrNil.
	aNode ifNotNil: [ aNode inStack: false ].
	^ aNode
]

{ #category : #actions }
DAPackageCycleDetector >> enqueue: aNode [
	queue nextPut: aNode
]

{ #category : #actions }
DAPackageCycleDetector >> findAllCyclesFrom: stronglyConnectedComponents for: package [
	| parents |
	self initializeVisitedNodes.
	parents := self parentsFrom: stronglyConnectedComponents For: package.
	self initializeQueueWith: package.
	 [ parents isEmpty ] whileFalse: [ self startBfsFrom: stronglyConnectedComponents For: parents ]
]

{ #category : #actions }
DAPackageCycleDetector >> findCycles: stronglyConnectedComponents [
	stronglyConnectedComponents packages
		do: [ :package |
			self findAllCyclesFrom: stronglyConnectedComponents for: package
			]
]

{ #category : #initialization }
DAPackageCycleDetector >> initialize [
	super initialize.
	cycles := SortedCollection new.
	sccs := OrderedCollection new
]

{ #category : #initialization }
DAPackageCycleDetector >> initializeQueueWith: package [

	package bfsParent: nil.
	package inStack: true.
	queue := WaitfreeQueue new.
	queue nextPut: package
]

{ #category : #initialization }
DAPackageCycleDetector >> initializeVisitedNodes [

	visitedNodes := Set new
]

{ #category : #initialization }
DAPackageCycleDetector >> initializeWithRPackageSet: aCollection [
	relationGraph := DAPackageRelationGraph onPackages: (aCollection collect: [ :each | DAPackage on: each ]).
	cycles := OrderedCollection new.
	self relationGraph build.
	sccs := self runTarjan
]

{ #category : #testing }
DAPackageCycleDetector >> isVisitedOrInQueue: aNode [
	^ (visitedNodes includes: aNode) or: [ aNode isInStack ]
]

{ #category : #accessing }
DAPackageCycleDetector >> parentsFrom: stronglyConnectedComponents For: aNode [
	^ stronglyConnectedComponents predecessors: aNode
]

{ #category : #accessing }
DAPackageCycleDetector >> refresh [
	self relationGraph refreshGraph.
	self
		reset;
		runAlgorithm
]

{ #category : #accessing }
DAPackageCycleDetector >> relationGraph [
	^ relationGraph
]

{ #category : #initialization }
DAPackageCycleDetector >> reset [
	cycles := SortedCollection new.
	visitedNodes := nil.
	queue := nil
]

{ #category : #actions }
DAPackageCycleDetector >> runAlgorithm [
	self stronglyConnectedComponents
		do: [ :scc | self findCycles: scc ]
		displayingProgress: 'finding cycles'
]

{ #category : #actions }
DAPackageCycleDetector >> runTarjan [

    ^ AITarjan new runOnDAPackageRelationGraph: self relationGraph
]

{ #category : #actions }
DAPackageCycleDetector >> startBfsFrom: stronglyConnectedComponents For: parents [
	| dequeue childrens |
	dequeue := self dequeue.
	childrens := self childrensFrom: stronglyConnectedComponents for: dequeue.
	childrens
		do: [ :children |
			(self isVisitedOrInQueue: children)
				ifFalse: [
					children bfsParent: dequeue.
					self enqueue: children ].
			(parents includes: children)
				ifTrue: [
					self buildCycleUntil: children.
					parents remove: children ] ].
	self addVisitedNodes: dequeue
]

{ #category : #accessing }
DAPackageCycleDetector >> stronglyConnectedComponents [
	^ sccs
]

{ #category : #accessing }
DAPackageCycleDetector >> visitedNodes [
	^ visitedNodes
]
