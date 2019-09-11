"
A PDPackageTreeModel is an abstract class. It's a the superclass of the two tree model vizualisations (package dependency and cycle detection).
"
Class {
	#name : #DAPackageTreeModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'tree',
		'buttonRefresh',
		'open',
		'browser'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #private }
DAPackageTreeModel >> actionOnRefresh [
	buttonRefresh action: [ self refresh ]
]

{ #category : #actions }
DAPackageTreeModel >> addDependenciesOptionsFor: selectedNodes on: menuGroup [
	| namesOfSelected |
	(self areClassToDependencyNodes: selectedNodes)
		ifTrue: [ 
			namesOfSelected := selectedNodes collect: [ :package | package content theClass ].
			menuGroup
				addMenuItem:
					(MenuItemPresenter new
						name: 'Browse dependencies from class';
						action: [ self openTreeFor: namesOfSelected ]) ]
]

{ #category : #actions }
DAPackageTreeModel >> addPackageOptionsFor: selectedNodes on: menuGroup [
	| selectedPackageNames |
	(self arePackageNodes: selectedNodes)
		ifTrue: [ 
			selectedPackageNames := selectedNodes collect: [ :packageNode | packageNode packageName ].
			menuGroup
				addMenuItem:
					(MenuItemPresenter new
						name: 'Browse scoped dependencies';
						action: [ (DAPackageAnalyzerPackageDependenciesWindow onPackagesNamed: selectedPackageNames) open ]).
			selectedNodes size > 1
				ifTrue: [ 
					menuGroup
						addMenuItem:
							(MenuItemPresenter new
								name: 'Find cycles among packages';
								action: [ (DAPackageAnalyzerCycleDetectionWindow onPackagesNamed: selectedPackageNames) open ]) ] ]
]

{ #category : #testing }
DAPackageTreeModel >> areClassToDependencyNodes: aCollectionOfItem [
	^ aCollectionOfItem allSatisfy: [ :node |  node isClassToDependencyNode ].
]

{ #category : #testing }
DAPackageTreeModel >> arePackageNodes: aCollectionOfItem [
	^ aCollectionOfItem allSatisfy: [ :node |  node isPackageNode ].
]

{ #category : #initialization }
DAPackageTreeModel >> browseReference [
	self selectedItemFromTree isReferenceNode ifTrue: [ 
			self selectedItemFromTree browseReference
		 ]
]

{ #category : #accessing }
DAPackageTreeModel >> browser [
	^ browser
]

{ #category : #private }
DAPackageTreeModel >> buildRoots [
	self subclassResponsibility.
]

{ #category : #accessing }
DAPackageTreeModel >> buttonRefresh [
	^ buttonRefresh
]

{ #category : #actions }
DAPackageTreeModel >> close [
	open := false.
	browser windowIsClosing
]

{ #category : #protocol }
DAPackageTreeModel >> defaultFormat [
	^ #png
]

{ #category : #testing }
DAPackageTreeModel >> ifGraphVizAbsent: aBlock [
	Smalltalk at: #GVGraphViz ifAbsent: aBlock
]

{ #category : #initialization }
DAPackageTreeModel >> initializePresenter [
	self actionOnRefresh
]

{ #category : #initialization }
DAPackageTreeModel >> initializeWidgets [
	tree := self newTree
		multiSelection: true;
		bindKeyCombination: $b meta toAction: [ self selectedItemFromTree spawnNewBrowser ];
		columns: {(TreeColumnPresenter new displayBlock: [ :node | node label ])};
		yourself.
	buttonRefresh := self newButton
		icon: (self iconNamed: #refreshIcon);
		help: 'Refresh the TreePresenter';
		label: 'Refresh';
		yourself.
	browser := Smalltalk tools browser onDefaultEnvironment
]

{ #category : #testing }
DAPackageTreeModel >> isOpen [
	^ open
]

{ #category : #actions }
DAPackageTreeModel >> loadMenuFor: item in: aMenu [
	| menuGroup selectedNodes |
	selectedNodes := self selectedItemsFromTree.
	menuGroup := MenuGroupPresenter new.
	self addPackageOptionsFor: selectedNodes on: menuGroup.
	self addDependenciesOptionsFor: selectedNodes asSet on: menuGroup.
	(selectedNodes size = 1)
		ifTrue: [ selectedNodes first addMenuEntriesOn: menuGroup ].
	
	aMenu addMenuGroup: menuGroup.
	^ aMenu
]

{ #category : #'instance creation' }
DAPackageTreeModel >> open [
	open := true.
	^ self openWithSpec 
]

{ #category : #actions }
DAPackageTreeModel >> openTreeFor: namesOfSelected [
	| treeToOpen relationGraph |
	treeToOpen := TreePresenter new.
	relationGraph := self tree owner relationGraph.
	^ treeToOpen
		roots: namesOfSelected;
		whenBuiltDo: [ 
					treeToOpen
						rootNodeHolder: [ :item | 
							DAClassToDependencyNode new
								content: item;
								package: (relationGraph pdPackageFrom: item) ] ];
		title: 'Class dependencies';
		extent: 700 @ 500;
		openWithSpec
]

{ #category : #actions }
DAPackageTreeModel >> refresh [
	self subclassResponsibility 
]

{ #category : #protocol }
DAPackageTreeModel >> saveGraphAndOpenWith: graphvizGenerator [
	graphvizGenerator saveDotFileAndGenerateForType: self defaultFormat.
	^ graphvizGenerator openWith: self defaultFormat
]

{ #category : #actions }
DAPackageTreeModel >> selectedItemFromTree [
	^ self tree selectedItem
]

{ #category : #accessing }
DAPackageTreeModel >> selectedItemsFromTree [
	^ self tree selectedItems
]

{ #category : #accessing }
DAPackageTreeModel >> tree [
	^ tree
]
