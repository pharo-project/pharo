"
I'm an abstract presenter.
I have the common UI definition of the dependency analyzer (left side panel, without the browser):
- main dependency analyzer window
- reverse dependency anlaysis
- and cycle detection
"
Class {
	#name : #DAPackageTreePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'tree',
		'buttonRefresh',
		'browser'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #testing }
DAPackageTreePresenter class >> isAbstract [

	^self == DAPackageTreePresenter
]

{ #category : #actions }
DAPackageTreePresenter >> addDependenciesOptionsFor: selectedNodes on: menuGroup [
	| namesOfSelected |
	(self areClassToDependencyNodes: selectedNodes)
		ifTrue: [
			namesOfSelected := selectedNodes collect: [ :package | package content theClass ].
			menuGroup
				addMenuItem:
					(SpMenuItemPresenter new
						name: 'Browse dependencies from class';
						action: [ self openTreeFor: namesOfSelected ]) ]
]

{ #category : #actions }
DAPackageTreePresenter >> addPackageOptionsFor: selectedNodes on: menuGroup [

	| selectedPackageNames |
	(self arePackageNodes: selectedNodes) ifFalse: [ ^ self ].

	selectedPackageNames := selectedNodes collect: [ :packageNode |
		                        packageNode packageName ].
	menuGroup addMenuItem: (SpMenuItemPresenter new
			 name: 'Browse scoped dependencies';
			 action: [
				 (DAPackageDependenciesPresenter onPackagesNamed:
						  selectedPackageNames) open ]).
	selectedNodes size > 1 ifTrue: [
		menuGroup addMenuItem: (SpMenuItemPresenter new
				 name: 'Find cycles among packages';
				 action: [
					 (DACycleDetectionPresenter onPackagesNamed: selectedPackageNames)
						 open ]) ]
]

{ #category : #testing }
DAPackageTreePresenter >> areClassToDependencyNodes: aCollectionOfItem [
	^ aCollectionOfItem allSatisfy: [ :node |  node isClassToDependencyNode ]
]

{ #category : #testing }
DAPackageTreePresenter >> arePackageNodes: aCollectionOfItem [
	^ aCollectionOfItem allSatisfy: [ :node |  node isPackageNode ]
]

{ #category : #initialization }
DAPackageTreePresenter >> browseReference [
	self selectedItemFromTree isReferenceNode ifTrue: [
			self selectedItemFromTree browseReference
		 ]
]

{ #category : #accessing }
DAPackageTreePresenter >> browser [
	^ browser
]

{ #category : #private }
DAPackageTreePresenter >> buildRoots [
	self subclassResponsibility
]

{ #category : #accessing }
DAPackageTreePresenter >> buttonRefresh [
	^ buttonRefresh
]

{ #category : #actions }
DAPackageTreePresenter >> close [
	browser windowIsClosing
]

{ #category : #initialization }
DAPackageTreePresenter >> connectPresenters [

	buttonRefresh action: [ self refresh ]
]

{ #category : #protocol }
DAPackageTreePresenter >> defaultFormat [
	^ #png
]

{ #category : #testing }
DAPackageTreePresenter >> ifGraphVizAbsent: aBlock [
	Smalltalk at: #GVGraphViz ifAbsent: aBlock
]

{ #category : #initialization }
DAPackageTreePresenter >> initializePresenters [
	tree := self newTree
		beMultipleSelection;
		bindKeyCombination: $b meta toAction: [ self selectedItemFromTree spawnNewBrowser ];
		children: [ :node | node buildChildren ];
		display: [ :node | node label ];
		displayIcon: [ :node | node icon ];
		yourself.

	buttonRefresh := self newButton
		icon: (self iconNamed: #refreshIcon);
		help: 'Refresh the TreePresenter';
		label: 'Refresh';
		yourself.

	browser := Smalltalk tools browser onDefaultEnvironment
]

{ #category : #actions }
DAPackageTreePresenter >> loadMenuForSelectedItem [

	| menu selectedNodes |
	menu := self newMenu.

	selectedNodes := self selectedItemsFromTree.
	menu addGroup: [ :menuGroup |
		self addPackageOptionsFor: selectedNodes on: menuGroup.
		self addDependenciesOptionsFor: selectedNodes asSet on: menuGroup.
		selectedNodes size = 1 ifTrue: [
			selectedNodes first addMenuEntriesOn: menuGroup ] ].
	^ menu
]

{ #category : #actions }
DAPackageTreePresenter >> openTreeFor: namesOfSelected [

	| treeToOpen relationGraph window |
	treeToOpen := self newTree.
	relationGraph := self tree owner relationGraph.
	window := treeToOpen
		          roots: (namesOfSelected collect: [ :name |
					           DAClassToDependencyNode new
						           content: name;
						           package: (relationGraph packageForClass: name) ]);
		          children: [ :node | node buildChildren ];
		          display: [ :node | node label ];
		          displayIcon: [ :node | node icon ];
		          asWindow.
	window
		title: 'Class dependencies';
		initialExtent: 700 @ 500;
		open
]

{ #category : #actions }
DAPackageTreePresenter >> refresh [
	self subclassResponsibility
]

{ #category : #protocol }
DAPackageTreePresenter >> saveGraphAndOpenWith: graphvizGenerator [
	graphvizGenerator saveDotFileAndGenerateForType: self defaultFormat.
	^ graphvizGenerator openWith: self defaultFormat
]

{ #category : #actions }
DAPackageTreePresenter >> selectedItemFromTree [
	^ self tree selectedItem
]

{ #category : #accessing }
DAPackageTreePresenter >> selectedItemsFromTree [
	^ self tree selectedItems
]

{ #category : #accessing }
DAPackageTreePresenter >> tree [
	^ tree
]
