"
A presenter for tree with a table used by the ProfilerUI to optimize rendering performance, but using more memory.
"
Class {
	#name : 'StProfilerTreeTablePresenter',
	#superclass : 'SpTreeTablePresenter',
	#instVars : [
		'viewModel'
	],
	#category : 'ProfilerUI-View',
	#package : 'ProfilerUI',
	#tag : 'View'
}

{ #category : 'specs' }
StProfilerTreeTablePresenter class >> adapterName [

	^ #ProfilerTreeTableAdapter
]

{ #category : 'api - selection' }
StProfilerTreeTablePresenter >> emptySelection [
	self withAdapterPerformOrDefer: [ :anAdapter |
		 anAdapter widget emptySelection ]
]

{ #category : 'expanding-collapsing' }
StProfilerTreeTablePresenter >> expandAllSuchThat: aBlock [

	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter widget expandAllSuchThat: aBlock ]
]

{ #category : 'expanding-collapsing' }
StProfilerTreeTablePresenter >> expandNodesOver: aNumber [
	self rememberingSelectedNodeDo: [
		self collapseAll.
		self expandAllSuchThat: [ :item | item weight asFloat >= aNumber asFloat ] ]
]

{ #category : 'expanding-collapsing' }
StProfilerTreeTablePresenter >> hideNodesUnder: aNumber [

	self rememberingExpandedAndSelectedNodesDo: [
		viewModel hideNodesUnder: aNumber asFloat.
		self reset ]
]

{ #category : 'initialization' }
StProfilerTreeTablePresenter >> initialize [
	super initialize.
	"the column below is created only to prevent the TreeTableMorph from creating an additional column"
	self addColumn: (SpCompositeTableColumn new
				   addColumn: (SpStringTableColumn
						    title: 'Method or Block'
						    evaluated: [ :item | item reportString ]);
				   yourself)
]

{ #category : 'expanding-collapsing' }
StProfilerTreeTablePresenter >> rememberingExpandedAndSelectedNodesDo: aBlock [

	self rememberingSelectedNodeDo: [
		self rememberingExpandedNodesDo: aBlock ]
]

{ #category : 'expanding-collapsing' }
StProfilerTreeTablePresenter >> rememberingExpandedNodesDo: aBlock [

	| expanded |
	self withAdapterPerformOrDefer: [ :anAdapter |
		expanded := anAdapter widget currentlyExpanded.
		aBlock value.
		expanded do: [ :n | anAdapter widget expandNodePath: n relativePath ] ]
]

{ #category : 'api - selection' }
StProfilerTreeTablePresenter >> rememberingSelectedNodeDo: aBlock [

	| selected widget |
	self withAdapterPerformOrDefer: [ :anAdapter |
		widget := anAdapter widget.
		selected := widget selectedMorph complexContents.
		aBlock value.

		self emptySelection.
		selected ifNotNil: [
			self selectPath: selected relativePath scrollToSelection: true ] ]
]

{ #category : 'updating' }
StProfilerTreeTablePresenter >> reset [
	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter widget buildContents ]
]

{ #category : 'api' }
StProfilerTreeTablePresenter >> roots: aCollection [
	super roots: aCollection.
	self emptySelection
]

{ #category : 'scrolling' }
StProfilerTreeTablePresenter >> scrollSelectionIntoView [
	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter widget scrollSelectionIntoView ]
]

{ #category : 'api - selection' }
StProfilerTreeTablePresenter >> selectNodePath: aCollection [

	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter widget selectNodePath: aCollection ]
]

{ #category : 'api - selection' }
StProfilerTreeTablePresenter >> selectPath: aPath scrollToSelection: shouldScrollToSelection [

	self selectNodePath: aPath.
	self withAdapterPerformOrDefer: [ :anAdapter |
		shouldScrollToSelection ifTrue: [ adapter scrollToSelection ] ]
]

{ #category : 'api - selection' }
StProfilerTreeTablePresenter >> selectedItem [

	self withAdapterPerformOrDefer: [ :anAdapter |
		^ anAdapter selectedItem ]
]

{ #category : 'accessing - model' }
StProfilerTreeTablePresenter >> setModelBeforeInitialization: aDomainObject [

	viewModel := aDomainObject
]

{ #category : 'accessing' }
StProfilerTreeTablePresenter >> viewModel [
	^ viewModel
]

{ #category : 'adapter' }
StProfilerTreeTablePresenter >> whenSelectedItemChangedSend: aSymbol to: anObject [

	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter widget model onSelectionChangeSend: aSymbol to: anObject ]
]
