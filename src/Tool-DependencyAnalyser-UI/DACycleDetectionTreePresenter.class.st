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
		'buttonSort'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #'instance creation' }
DACycleDetectionTreePresenter class >> onPackagesMatch: match [
	^ (self onPackagesNamed:
			(Smalltalk organization packages
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
		(Smalltalk organization packages collect: [ :package | package packageName asString ])
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
	| roots |
	roots := cycles collect: [ :cycle |
		DACycleNode new
			content: cycle;
			treeModelParent: self;
			browser: self browser;
			cycle: true ].
	self tree roots: roots.

	cyclesLabel
		label: 'Analysis of ' , self cycles size asString , ' cycles'
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> cycles [
	^ cycles
]

{ #category : #accessing }
DACycleDetectionTreePresenter >> cycles: aCollectionOfCycles [
	cycles := aCollectionOfCycles sorted: [ :a :b | a size > b size ].
	self buildRoots
]

{ #category : #layout }
DACycleDetectionTreePresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		add: (SpBoxLayout newLeftToRight
				add: buttonSort;
				add: buttonRefresh;
			yourself)
			expand: false;
		add: 'Analysis of cycles' expand: false;
		add: tree;
		yourself
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

	tree whenSelectedItemChangedDo: [ :item | | menu |
		item ifNotNil: [
			menu := self newMenu.
			item addMenuEntriesOn: menu.
			tree contextMenu: menu. ] ].

	cyclesLabel := self newLabel
			label: 'Analysis of cycles'.

	buttonSort := self newButton
			help: 'Sort cycles by the number of packages involved in the cycle';
			label: 'Sort by length';
			action: [ self reversedRoots  ].

	self layout: self defaultLayout
]

{ #category : #initialization }
DACycleDetectionTreePresenter >> initializeWithPackageName: aCollection [

	self analysis:  (DAPackageCycleDetector onPackagesNamed: aCollection) runAlgorithm.

	self cycles: self analysis cycles
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
	self tree roots: (self tree roots reversed)
]

{ #category : #protocol }
DACycleDetectionTreePresenter >> title [
	^ 'Cycles analysis'
]
