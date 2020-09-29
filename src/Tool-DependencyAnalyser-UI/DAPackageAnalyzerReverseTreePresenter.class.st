"
A PDPackageAnalyzerReverseTreeModel shows all users from a set of packages. Also you can see all type of the dependency, and browse them.

The window is initialized with a relation graph of package dependencies, with all the dependencies already compute.
"
Class {
	#name : #DAPackageAnalyzerReverseTreePresenter,
	#superclass : #DAPackageTreePresenter,
	#instVars : [
		'relationGraph',
		'packageLabel'
	],
	#category : #'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #specs }
DAPackageAnalyzerReverseTreePresenter class >> defaultSpec [
	<spec: #default>
	^ SpecLayout composed
		newColumn:
			[ :col | 
			col
				newRow: [ :row | row add: #buttonRefresh ] height: 30;
				newRow: [ :row | 
							row
								add: #packageLabel ]
					height: 30;
				add: #tree ] yourself
]

{ #category : #'instance creation' }
DAPackageAnalyzerReverseTreePresenter class >> onRelationGraph: aRelationGraph [
	^ self new
		initializeWithRelationGraph: aRelationGraph;
		yourself
]

{ #category : #private }
DAPackageAnalyzerReverseTreePresenter >> buildRoots [
	| collectionOfWrapper |
	collectionOfWrapper := relationGraph allDependentPackagesFromSeenPackages
		collect: [ :package | package asDependentPackageWrapperOn: relationGraph ].
	
	self tree roots: (self nodesFor: collectionOfWrapper asSortedCollection).
	self updatePackageLabel.
]

{ #category : #initialization }
DAPackageAnalyzerReverseTreePresenter >> initializePresenter [
	super initializePresenter.
	
	tree
		whenSelectedItemChanged: [ :item | 
			item
				ifNotNil: [ 
					self tree selectedItem selectInBrowser.
					tree menu: [ :aMenu | self loadMenuFor: item in: aMenu ] ] ].
]

{ #category : #initialization }
DAPackageAnalyzerReverseTreePresenter >> initializeWidgets [
	super initializeWidgets.
	
	packageLabel := self newLabel label: 'Analysis of packages'.
]

{ #category : #initialization }
DAPackageAnalyzerReverseTreePresenter >> initializeWithRelationGraph: aRelationGraph [
	relationGraph := aRelationGraph.
	self buildRoots.
]

{ #category : #private }
DAPackageAnalyzerReverseTreePresenter >> nodesFor: anItemList [
	^ anItemList collect: 
		[ :each | 
		DAReversePackageNode new
						content: each;
						treeModelParent: self;
						browser: self browser;
						cycle: false;
						yourself ]
]

{ #category : #accessing }
DAPackageAnalyzerReverseTreePresenter >> packageLabel [
	^ packageLabel
]

{ #category : #actions }
DAPackageAnalyzerReverseTreePresenter >> refresh [
		self isOpen ifTrue: [  
			self relationGraph refreshGraph.
			self buildRoots].
]

{ #category : #accessing }
DAPackageAnalyzerReverseTreePresenter >> relationGraph [
	^ relationGraph
]

{ #category : #api }
DAPackageAnalyzerReverseTreePresenter >> title [
	^ 'Package Dependencies Reverse Analysis'
]

{ #category : #private }
DAPackageAnalyzerReverseTreePresenter >> updatePackageLabel [
	self packageLabel label: 'Analysis of ' , self tree roots size asString , ' package(s)'
]
