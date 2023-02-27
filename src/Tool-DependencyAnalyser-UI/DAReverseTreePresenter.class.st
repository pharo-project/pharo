"
A DAPackageAnalyzerReverseTreeModel shows all users from a set of packages. Also you can see all type of the dependency, and browse them.

The presenter is initialized with a relation graph of package dependencies, with all the dependencies already compute.
"
Class {
	#name : #DAReverseTreePresenter,
	#superclass : #DAPackageTreePresenter,
	#instVars : [
		'relationGraph',
		'packageLabel'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #'instance creation' }
DAReverseTreePresenter class >> onRelationGraph: aRelationGraph [
	^ self on: aRelationGraph
]

{ #category : #private }
DAReverseTreePresenter >> buildRoots [
	| collectionOfWrapper |
	collectionOfWrapper := relationGraph allDependentPackagesFromSeenPackages
		collect: [ :package | package asDependentPackageWrapperOn: relationGraph ].

	self tree roots: (self nodesFor: collectionOfWrapper asSortedCollection).
	self updatePackageLabel
]

{ #category : #initialization }
DAReverseTreePresenter >> connectPresenters [
	super connectPresenters.

	tree
		whenSelectedItemChangedDo: [ :item |
			item
				ifNotNil: [
					self tree selectedItem selectInBrowser.
					tree contextMenu: [ :aMenu | self loadMenuForSelectedItem ] ] ]
]

{ #category : #layout }
DAReverseTreePresenter >> defaultLayout [

	^ SpBoxLayout newTopToBottom
		  add: buttonRefresh expand: false;
		  add: packageLabel expand: false;
		  add: tree;
		  yourself
]

{ #category : #initialization }
DAReverseTreePresenter >> initializePresenters [

	super initializePresenters.

	packageLabel := self newLabel label: 'Analysis of packages'.
	self buildRoots
]

{ #category : #private }
DAReverseTreePresenter >> nodesFor: anItemList [
	^ anItemList collect:
		[ :each |
		DAReversePackageNode new
						content: each;
						treeModelParent: self;
						browser: self browser;
						cycle: false;
						yourself ]
]

{ #category : #actions }
DAReverseTreePresenter >> refresh [

	self relationGraph refreshGraph.
	self buildRoots
]

{ #category : #accessing }
DAReverseTreePresenter >> relationGraph [
	^ relationGraph
]

{ #category : #initialization }
DAReverseTreePresenter >> setModelBeforeInitialization: aRelationGraph [
	relationGraph := aRelationGraph
]

{ #category : #accessing }
DAReverseTreePresenter >> title [
	^ 'Package Dependencies Reverse Analysis'
]

{ #category : #private }
DAReverseTreePresenter >> updatePackageLabel [
	packageLabel label: 'Analysis of ' , self tree roots size asString , ' package(s)'
]
