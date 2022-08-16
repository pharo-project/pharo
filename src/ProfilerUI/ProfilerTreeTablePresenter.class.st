"
A presenter for tree with a table used by the ProfilerUI to optimize rendering performance, but using more memory.
"
Class {
	#name : #ProfilerTreeTablePresenter,
	#superclass : #SpTreeTablePresenter,
	#instVars : [
		'viewModel'
	],
	#category : #'ProfilerUI-View'
}

{ #category : #specs }
ProfilerTreeTablePresenter class >> adapterName [ 

	^ #ProfilerTreeTableAdapter
]

{ #category : #selection }
ProfilerTreeTablePresenter >> emptySelection [
	self withAdapterPerformOrDefer: [ :anAdapter |
		 anAdapter widget emptySelection ]
]

{ #category : #'expanding-collapsing' }
ProfilerTreeTablePresenter >> expandAllSuchThat: aBlock [

	self withAdapterPerformOrDefer: [ :anAdapter | 
		anAdapter widget expandAllSuchThat: aBlock ]
]

{ #category : #'expanding-collapsing' }
ProfilerTreeTablePresenter >> expandNodesOver: aNumber [
	self rememberingSelectedNodeDo: [ 
		self collapseAll.
		self expandAllSuchThat: [ :item | item weight asFloat >= aNumber asFloat ] ]
]

{ #category : #'expanding-collapsing' }
ProfilerTreeTablePresenter >> hideNodesUnder: aNumber [

	self rememberingExpandedAndSelectedNodesDo: [ 
		viewModel hideNodesUnder: aNumber asFloat.
		self updateResults ]
]

{ #category : #initialization }
ProfilerTreeTablePresenter >> initialize [
	super initialize.
	"the column below is created only to prevent the TreeTableMorph from creating an additional column"
	self addColumn: (SpCompositeTableColumn new
				   addColumn: (SpStringTableColumn
						    title: 'Method or Block'
						    evaluated: [ :item | item reportString ]);
				   yourself)
]

{ #category : #'expanding-collapsing' }
ProfilerTreeTablePresenter >> rememberingExpandedAndSelectedNodesDo: aBlock [

	self rememberingSelectedNodeDo: [ 
		self rememberingExpandedNodesDo: aBlock ]
]

{ #category : #'expanding-collapsing' }
ProfilerTreeTablePresenter >> rememberingExpandedNodesDo: aBlock [

	| expanded |
	self withAdapterPerformOrDefer: [ :anAdapter | 
		expanded := anAdapter widget currentlyExpanded.
		aBlock value.
		expanded do: [ :n | anAdapter widget expandNodePath: n path ] ]
]

{ #category : #selection }
ProfilerTreeTablePresenter >> rememberingSelectedNodeDo: aBlock [

	| selected widget |
	self withAdapterPerformOrDefer: [ :anAdapter | 
		widget := anAdapter widget.
		selected := widget selectedMorph complexContents.
		aBlock value.

		widget emptySelection.
		selected ifNotNil: [ 
			widget selectNodePath: selected path. 
			widget scrollSelectionIntoView ] ]
]

{ #category : #updating }
ProfilerTreeTablePresenter >> resetContents [
	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter widget buildContents ]
]

{ #category : #scrolling }
ProfilerTreeTablePresenter >> scrollSelectionIntoView [
	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter widget scrollSelectionIntoView ]
]

{ #category : #selection }
ProfilerTreeTablePresenter >> selectNodePath: aCollection [ 

	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter widget selectNodePath: aCollection ]
]

{ #category : #'api - selection' }
ProfilerTreeTablePresenter >> selectPath: aPath scrollToSelection: shouldScrollToSelection [
	self selectNodePath: aPath.
	self withAdapterPerformOrDefer: [ :anAdapter |
		
		shouldScrollToSelection ifTrue: [ 
			adapter scrollToSelection ] ]
]

{ #category : #'api - selection' }
ProfilerTreeTablePresenter >> selectedItem [

	self withAdapterPerformOrDefer: [ :anAdapter | 
		^ anAdapter selectedItem ]
]

{ #category : #'accessing - model' }
ProfilerTreeTablePresenter >> setModelBeforeInitialization: aDomainObject [

	viewModel := aDomainObject
]

{ #category : #'expanding-collapsing' }
ProfilerTreeTablePresenter >> updateResults [

	self roots: {viewModel rootItems}.
	self refresh
]

{ #category : #accessing }
ProfilerTreeTablePresenter >> viewModel [
	^ viewModel
]

{ #category : #adapter }
ProfilerTreeTablePresenter >> whenSelectedItemChangedSend: aSymbol to: anObject [

	self withAdapterPerformOrDefer: [ :anAdapter | 
		anAdapter widget model onSelectionChangeSend: aSymbol to: anObject ]
]
