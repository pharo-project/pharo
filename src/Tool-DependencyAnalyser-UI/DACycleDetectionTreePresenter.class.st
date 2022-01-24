"
A PDPackageDetectCycleTreeModel shows all cycles among packages from a graph of dependencies.
"
Class {
	#name : #DACycleDetectionTreePresenter,
	#superclass : #DAPackageTreePresenter,
	#instVars : [
		'analysis',
		'cycles',
		'cyclesLabel',
		'filter',
		'buttonReversed'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #specs }
DACycleDetectionTreePresenter class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :col | 
			col
				newRow: [ :row | 
							row
								add: #buttonReversed;
								add: #buttonRefresh ]
					height: 30;
				newRow: [ :row | 
							row
								add: #cyclesLabel ]
					height: 30;
				add: #tree ] yourself
]

{ #category : #'instance creation' }
DACycleDetectionTreePresenter class >> onPackagesMatch: match [
	^ (self onPackagesNamed:
			(RPackageOrganizer default packages
				select: [ :package | match match: package packageName asString ]
				thenCollect: [ :package | package packageName ]) ) 
]

{ #category : #'instance creation' }
DACycleDetectionTreePresenter class >> onPackagesNamed: aCollection [
	^ self new 
		initializeWithPackageName: aCollection;
		yourself
]

{ #category : #'instance creation' }
DACycleDetectionTreePresenter class >> system [
	^ self onPackagesNamed: 
		(RPackageOrganizer default packages collect: [ :package | package packageName asString ])
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> analysis [
	^ analysis
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> analysis: anObject [
	analysis := anObject
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> browser: anObject [
	browser := anObject
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> buildRoots [
	self tree
		roots: cycles;
		whenBuiltDo: [ 
					tree
						rootNodeHolder: [ :item | 
							DACycleNode new
								content: item;
								treeModelParent: self;
								browser: self browser;
								cycle: true ] ].
	self cyclesLabel
		label:
			'Analysis of ' , self cycles size asString , ' cycles'.
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> buttonReversed [
	^ buttonReversed
]

{ #category : #initialization }
DACycleDetectionTreePresenter >> connectPresenters [
	super connectPresenters.
	
	cyclesLabel := self newLabel
			label: 'Analysis of cycles'.
	
	buttonReversed := self newButton
			help: 'Reversed the order of cycles';
			label: 'Sort by length'.
	
		
	
	
		
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> cycles [
	^ cycles
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> cycles: aCollectionOfCycles [
	cycles := aCollectionOfCycles sorted: [ :a :b | a size > b size ].
	self buildRoots.
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> cyclesLabel [
	^ cyclesLabel
]

{ #category : #protocol }
DACycleDetectionTreePresenter >> extent [
	^ 1000@600
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> filter [
	^ filter
]

{ #category : #filtering }
DACycleDetectionTreePresenter >> filterWithPackages [
	
	self cycles: (self analysis cyclesWithAtLeast: filter)
]

{ #category : #filtering }
DACycleDetectionTreePresenter >> filterWithPackages: aCollectionOfPDPackage [
	filter := aCollectionOfPDPackage.
	self cycles: (self analysis cyclesWithAtLeast: filter)
]

{ #category : #initialization }
DACycleDetectionTreePresenter >> initializePresenters [
	super initializePresenters.
	
	cyclesLabel := self newLabel
			label: 'Analysis of cycles'.
	
	buttonReversed := self newButton
			help: 'Reversed the order of cycles';
			label: 'Sort by length'
]

{ #category : #initialization }
DACycleDetectionTreePresenter >> initializeWithPackageName: aCollection [
	
	self analysis:  (DAPackageCycleDetector onPackagesNamed: aCollection) runAlgorithm.
	
	self cycles: self analysis cycles.
]

{ #category : #actions }
DACycleDetectionTreePresenter >> refresh [
	self analysis refresh.
	self filter ifNil: [ self cycles: self analysis cycles ]
		ifNotNil: [ self cycles: (self analysis cyclesWithAtLeast: filter) ].
	self buildRoots
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> reversedRoots [
	self tree roots: (self tree roots reversed).
]

{ #category : #protocol }
DACycleDetectionTreePresenter >> title [
	^ 'Cycles analysis'
]
