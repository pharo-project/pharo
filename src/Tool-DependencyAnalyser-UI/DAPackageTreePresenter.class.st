"
I'm an abstract presenter.
I have the common UI definition of the dependency analyzer (left side panel, without the browser):
- main dependency analyzer window
- reverse dependency anlaysis
- and cycle detection
"
Class {
	#name : 'DAPackageTreePresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'tree',
		'buttonRefresh',
		'browser'
	],
	#category : 'Tool-DependencyAnalyser-UI-Core',
	#package : 'Tool-DependencyAnalyser-UI',
	#tag : 'Core'
}

{ #category : 'testing' }
DAPackageTreePresenter class >> isAbstract [

	^self == DAPackageTreePresenter
]

{ #category : 'testing' }
DAPackageTreePresenter >> areClassToDependencyNodes: aCollectionOfItem [
	^ aCollectionOfItem allSatisfy: [ :node |  node isClassToDependencyNode ]
]

{ #category : 'testing' }
DAPackageTreePresenter >> arePackageNodes: aCollectionOfItem [

	^ aCollectionOfItem allSatisfy: [ :node |  node isPackageNode ]
]

{ #category : 'initialization' }
DAPackageTreePresenter >> browseReference [
	self selectedItemFromTree isReferenceNode ifTrue: [
			self selectedItemFromTree browseReference
		 ]
]

{ #category : 'accessing' }
DAPackageTreePresenter >> browser [
	^ browser
]

{ #category : 'private' }
DAPackageTreePresenter >> buildRoots [
	self subclassResponsibility
]

{ #category : 'accessing' }
DAPackageTreePresenter >> buttonRefresh [
	^ buttonRefresh
]

{ #category : 'actions' }
DAPackageTreePresenter >> close [
	browser windowIsClosing
]

{ #category : 'initialization' }
DAPackageTreePresenter >> connectPresenters [

	buttonRefresh action: [ self refresh ]
]

{ #category : 'protocol' }
DAPackageTreePresenter >> defaultFormat [
	^ #png
]

{ #category : 'initialization' }
DAPackageTreePresenter >> initializePresenters [

	tree := self newTree	
		beMultipleSelection;
		addShortcutWith: [ :action | action 
			shortcutKey: $b actionModifier;
			action: [ self selectedItemFromTree spawnNewBrowser ] ];
		children: [ :node | node buildChildren ];
		display: [ :node | node label ];
		displayIcon: [ :node | node icon ];
		expandRoots; "<= We want to open the browser with the dependencies displayed to the user"
		yourself.

	buttonRefresh := self newButton
		icon: (self iconNamed: #refresh);
		label: 'Refresh';
		help: 'Refresh the list of analyzed packages';
		yourself.

	browser := (self application toolNamed: #browser) onDefaultEnvironment
]

{ #category : 'actions' }
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

{ #category : 'actions' }
DAPackageTreePresenter >> refresh [
	self subclassResponsibility
]

{ #category : 'actions' }
DAPackageTreePresenter >> selectedItemFromTree [
	^ self tree selectedItem
]

{ #category : 'accessing' }
DAPackageTreePresenter >> selectedItemsFromTree [
	^ self tree selectedItems
]

{ #category : 'accessing' }
DAPackageTreePresenter >> tree [
	^ tree
]
