"
A PDPackageDetectCycleTreeModel shows all cycles among packages from a graph of dependencies.
"
Class {
	#name : #DAPackageCycleDetectionTreePresenter,
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
DAPackageCycleDetectionTreePresenter class >> defaultSpec [
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
DAPackageCycleDetectionTreePresenter class >> onPackagesMatch: match [
	^ (self onPackagesNamed:
			(RPackageOrganizer default packages
				select: [ :package | match match: package packageName asString ]
				thenCollect: [ :package | package packageName ]) ) 
]

{ #category : #'instance creation' }
DAPackageCycleDetectionTreePresenter class >> onPackagesNamed: aCollection [
	^ self new 
		initializeWithPackageName: aCollection;
		yourself
]

{ #category : #'instance creation' }
DAPackageCycleDetectionTreePresenter class >> system [
	^ self onPackagesNamed: 
		(RPackageOrganizer default packages collect: [ :package | package packageName asString ])
]

{ #category : #accessing }
DAPackageCycleDetectionTreePresenter >> analysis [
	^ analysis
]

{ #category : #accessing }
DAPackageCycleDetectionTreePresenter >> analysis: anObject [
	analysis := anObject
]

{ #category : #accessing }
DAPackageCycleDetectionTreePresenter >> browser: anObject [
	browser := anObject
]

{ #category : #accessing }
DAPackageCycleDetectionTreePresenter >> buildRoots [
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
DAPackageCycleDetectionTreePresenter >> buttonReversed [
	^ buttonReversed
]

{ #category : #accessing }
DAPackageCycleDetectionTreePresenter >> cycles [
	^ cycles
]

{ #category : #accessing }
DAPackageCycleDetectionTreePresenter >> cycles: aCollectionOfCycles [
	cycles := aCollectionOfCycles sorted: [ :a :b | a size > b size ].
	self buildRoots.
]

{ #category : #accessing }
DAPackageCycleDetectionTreePresenter >> cyclesLabel [
	^ cyclesLabel
]

{ #category : #protocol }
DAPackageCycleDetectionTreePresenter >> extent [
	^ 1000@600
]

{ #category : #accessing }
DAPackageCycleDetectionTreePresenter >> filter [
	^ filter
]

{ #category : #filtering }
DAPackageCycleDetectionTreePresenter >> filterWithPackages [
	
	self cycles: (self analysis cyclesWithAtLeast: filter)
]

{ #category : #filtering }
DAPackageCycleDetectionTreePresenter >> filterWithPackages: aCollectionOfPDPackage [
	filter := aCollectionOfPDPackage.
	self cycles: (self analysis cyclesWithAtLeast: filter)
]

{ #category : #initialization }
DAPackageCycleDetectionTreePresenter >> initializePresenter [
	super initializePresenter.
	
	buttonReversed action: [ self reversedRoots  ].
	
	
	
	tree
		whenSelectedItemChanged: [ :item | 
			item
				ifNotNil: [ 
					self tree selectedItem selectInBrowser.
					tree menu: [ :aMenu | self loadMenuFor: item in: aMenu ] ] ]
]

{ #category : #initialization }
DAPackageCycleDetectionTreePresenter >> initializeWidgets [
	super initializeWidgets.
	
	cyclesLabel := self newLabel
			label: 'Analysis of cycles'.
	
	buttonReversed := self newButton
			help: 'Reversed the order of cycles';
			label: 'Sort by length'.
	
		
	
	
		
]

{ #category : #initialization }
DAPackageCycleDetectionTreePresenter >> initializeWithPackageName: aCollection [
	
	self analysis:  (DAPackageCycleDetector onPackagesNamed: aCollection) runAlgorithm.
	
	self cycles: self analysis cycles.
]

{ #category : #actions }
DAPackageCycleDetectionTreePresenter >> refresh [
	self analysis refresh.
	self filter ifNil: [ self cycles: self analysis cycles ]
		ifNotNil: [ self cycles: (self analysis cyclesWithAtLeast: filter) ].
	self buildRoots
]

{ #category : #accessing }
DAPackageCycleDetectionTreePresenter >> reversedRoots [
	self tree roots: (self tree roots reversed).
]

{ #category : #protocol }
DAPackageCycleDetectionTreePresenter >> title [
	^ 'Cycles analysis'
]
