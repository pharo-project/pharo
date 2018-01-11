"
A PDPackageDetectCycleTreeModel shows all cycles among packages from a graph of dependencies.
"
Class {
	#name : #DAPackageCycleDetectionTreeModel,
	#superclass : #DAPackageTreeModel,
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
DAPackageCycleDetectionTreeModel class >> defaultSpec [
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

{ #category : #'instance-creation' }
DAPackageCycleDetectionTreeModel class >> onPackagesMatch: match [
	^ (self onPackagesNamed:
			(RPackageOrganizer default packages
				select: [ :package | match match: package packageName asString ]
				thenCollect: [ :package | package packageName ]) ) 
]

{ #category : #'instance-creation' }
DAPackageCycleDetectionTreeModel class >> onPackagesNamed: aCollection [
	^ self new 
		initializeWithPackageName: aCollection;
		yourself
]

{ #category : #'instance-creation' }
DAPackageCycleDetectionTreeModel class >> system [
	^ self onPackagesNamed: 
		(RPackageOrganizer default packages collect: [ :package | package packageName asString ])
]

{ #category : #accessing }
DAPackageCycleDetectionTreeModel >> analysis [
	^ analysis
]

{ #category : #accessing }
DAPackageCycleDetectionTreeModel >> analysis: anObject [
	analysis := anObject
]

{ #category : #accessing }
DAPackageCycleDetectionTreeModel >> browser: anObject [
	browser := anObject
]

{ #category : #accessing }
DAPackageCycleDetectionTreeModel >> buildRoots [
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
DAPackageCycleDetectionTreeModel >> buttonReversed [
	^ buttonReversed
]

{ #category : #accessing }
DAPackageCycleDetectionTreeModel >> cycles [
	^ cycles
]

{ #category : #accessing }
DAPackageCycleDetectionTreeModel >> cycles: aCollectionOfCycles [
	cycles := aCollectionOfCycles sorted: [ :a :b | a size > b size ].
	self buildRoots.
]

{ #category : #accessing }
DAPackageCycleDetectionTreeModel >> cyclesLabel [
	^ cyclesLabel
]

{ #category : #protocol }
DAPackageCycleDetectionTreeModel >> extent [
	^ 1000@600
]

{ #category : #accessing }
DAPackageCycleDetectionTreeModel >> filter [
	^ filter
]

{ #category : #filtering }
DAPackageCycleDetectionTreeModel >> filterWithPackages [
	
	self cycles: (self analysis cyclesWithAtLeast: filter)
]

{ #category : #filtering }
DAPackageCycleDetectionTreeModel >> filterWithPackages: aCollectionOfPDPackage [
	filter := aCollectionOfPDPackage.
	self cycles: (self analysis cyclesWithAtLeast: filter)
]

{ #category : #initialization }
DAPackageCycleDetectionTreeModel >> initializePresenter [
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
DAPackageCycleDetectionTreeModel >> initializeWidgets [
	super initializeWidgets.
	
	cyclesLabel := self newLabel
			label: 'Analysis of cycles'.
	
	buttonReversed := self newButton
			help: 'Reversed the order of cycles';
			label: 'Sort by length'.
	
		
	
	
		
]

{ #category : #initialization }
DAPackageCycleDetectionTreeModel >> initializeWithPackageName: aCollection [
	
	self analysis:  (DAPackageCycleDetector onPackagesNamed: aCollection) runAlgorithm.
	
	self cycles: self analysis cycles.
]

{ #category : #actions }
DAPackageCycleDetectionTreeModel >> refresh [
	self analysis refresh.
	self filter ifNil: [ self cycles: self analysis cycles ]
		ifNotNil: [ self cycles: (self analysis cyclesWithAtLeast: filter) ].
	self buildRoots
]

{ #category : #accessing }
DAPackageCycleDetectionTreeModel >> reversedRoots [
	self tree roots: (self tree roots reversed).
]

{ #category : #protocol }
DAPackageCycleDetectionTreeModel >> title [
	^ 'Cycles analysis'
]
