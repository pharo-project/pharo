"
A presenter to display tree of elements.
A `SpTreePresenter` will display a line with a description of the element (See `SpTreePresenter>>#display:`, and optionally an icon (See `SpTreePresenter>>#icon:`).

A `SpTreePresenter` can be seen as a simplified `SpTreeTablePresenter`.

"
Class {
	#name : #SpTreePresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#traits : 'SpTHaveWrappingScrollBars + SpTContextMenu + SpTSearchable',
	#classTraits : 'SpTHaveWrappingScrollBars classTrait + SpTContextMenu classTrait + SpTSearchable classTrait',
	#instVars : [
		'#showColumnHeaders => SpObservableSlot',
		'#isResizable => SpObservableSlot',
		'#roots => SpObservableSlot',
		'#selectionMode',
		'#childrenBlock',
		'#activationBlock',
		'#activateOnSingleClick',
		'#verticalAlignment',
		'#iconBlock',
		'#displayBlock'
	],
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #specs }
SpTreePresenter class >> adapterName [

	^ #TreeAdapter
]

{ #category : #documentation }
SpTreePresenter class >> documentFactoryMethodSelector [

	^ #newTree
]

{ #category : #documentation }
SpTreePresenter class >> documentSections [
		
	^ OrderedDictionary newFromPairs: {
		'Examples'. self class methods select: [ :each | each protocol = self documentExamplesProtocol ].
		'API Methods'. self methods select: [ :each | each protocol = #'api' ].
		'API Selection Methods'. self methods select: [ :each | each protocol = #'api-selection' ].
		'Testing Methods'. self methods select: [ :each | each protocol = #'testing' ].
		'Events'. self methods select: [ :each | each protocol = #'api-events' ].
	 }
]

{ #category : #api }
SpTreePresenter >> activateOnDoubleClick [
	"Configure the list to trigger activation on double click.
	 An element on a list can be 'activated', meaning it will trigger an event to execute an 
	 action on it. An activation is different than a selection (one can 'select' an element without 
	 activating it).
	 See `SpTreeTablePresenter>>#whenActivatedDo:`"
	
	activateOnSingleClick := false
]

{ #category : #api }
SpTreePresenter >> activateOnSingleClick [
	"Configure the list to trigger activation on single click.
	 An element on a list can be 'activated', meaning it will trigger an event to execute an 
	 action on it. An activation is different than a selection (one can 'select' an element without 
	 activating it).
	 See `SpTreeTablePresenter>>#whenActivatedDo:`"
	
	activateOnSingleClick := true
]

{ #category : #testing }
SpTreePresenter >> activatesOnDoubleClick [
	"Answer true if activation event is triggered on double click"
	
	^ activateOnSingleClick not
]

{ #category : #testing }
SpTreePresenter >> activatesOnSingleClick [
	"Answer true if activation event is triggered on single click"

	^ activateOnSingleClick
]

{ #category : #api }
SpTreePresenter >> beMultipleSelection [
	"Enable multiple selection."

	self selectionMode: (SpTreeMultipleSelectionMode on: self)
]

{ #category : #api }
SpTreePresenter >> beNotResizable [
	"Mark the table as 'not resizable', which means there will be not possibility to resize the 
	 columns of it."

	self isResizable: false
]

{ #category : #api }
SpTreePresenter >> beResizable [
	"Mark the table as 'resizable', which means there will be a slider to resize the columns."

	self isResizable: true
]

{ #category : #api }
SpTreePresenter >> beSingleSelection [
	"Enable single selection (this is the default)."
	
	self selectionMode: (SpTreeSingleSelectionMode on: self)
]

{ #category : #private }
SpTreePresenter >> children [

	^ childrenBlock
]

{ #category : #api }
SpTreePresenter >> children: aBlock [
	"Set a block to answer the children of a node when it is expanded.
	 `aBlock` receives one argument, the node element to expand.
	 If there are no children to answer, `aBlock` needs to answer an empty collection."		

	childrenBlock := aBlock
]

{ #category : #private }
SpTreePresenter >> childrenFor: anObject [ 

	^ self children value: anObject
]

{ #category : #simulation }
SpTreePresenter >> clickAtPath: aPath [

	self selectPath: aPath.

	activateOnSingleClick ifFalse: [ ^ self ].
	self doActivateAtPath: aPath
]

{ #category : #api }
SpTreePresenter >> display [
	"Answer the display block that will transform the model nodes into a displayable string."

	^ displayBlock
]

{ #category : #api }
SpTreePresenter >> display: aBlock [
	"Set the block that will be applied on each of the list items. 
	 The result of the block will be used to display the item on the screen.
	 `aBlock` receives one argument.
	 Here is the typical example:

	 initializePresenters
   		...
   		tree := self newTree.
		tree display: [ :anObject | anObject printString ]
   		...
	 "

	displayBlock := aBlock
]

{ #category : #api }
SpTreePresenter >> displayIcon [
	"Return the block used to return an icon that will be displayed in the tree"

	^ iconBlock
]

{ #category : #api }
SpTreePresenter >> displayIcon: aBlock [
	"Set a block which takes an item as argument and returns the icon to display in the tree.
	 `aBlock` receives one argument"
	
	iconBlock := aBlock
]

{ #category : #simulation }
SpTreePresenter >> doActivateAtPath: aPath [
	"Activate only if there is an item at that position"

	self itemAtPath: aPath ifAbsent: [ ^ self ].
	activationBlock
		cull:
			((SpTreeSingleSelectionMode on: self)
				selectPath: aPath;
				yourself)
]

{ #category : #simulation }
SpTreePresenter >> doubleClickAtPath: aPath [
	self selectPath: aPath.
	
	activateOnSingleClick ifTrue: [ ^ self ].
	self doActivateAtPath: aPath
]

{ #category : #api }
SpTreePresenter >> expandPath: aPath [
	"Expand the tree path.
	`aPath` is the path to expand. A path is an array of node indexes (e.g. #(1 2 3))"

	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter expandPath: aPath ]
]

{ #category : #api }
SpTreePresenter >> expandRoots [
	"Expand all roots of the tree"

	self withAdapterPerformOrDefer: [ :anAdapter | 
		anAdapter expandRoots ]
]

{ #category : #api }
SpTreePresenter >> hideColumnHeaders [
	"Hide the column headers"

	showColumnHeaders := false
]

{ #category : #initialization }
SpTreePresenter >> initialize [
	super initialize.
	self initializeTSearchable.

	self withScrollBars.
	
	activationBlock := [ ].
	
	displayBlock := [ :each | each asString ].
	showColumnHeaders := true.
	isResizable := false.
	childrenBlock := [ :item | #() ].
	roots := #().
	verticalAlignment := SpVerticalAlignment new.

	self beSingleSelection.
	self activateOnDoubleClick.
	self registerEvents
]

{ #category : #testing }
SpTreePresenter >> isResizable [
	"Answer true if table allows resizing of its columns."

	^ isResizable
]

{ #category : #private }
SpTreePresenter >> isResizable: aBoolean [
	isResizable := aBoolean
]

{ #category : #testing }
SpTreePresenter >> isShowingColumnHeaders [
	"Answer true if the table is configured to show column headers."

	^ showColumnHeaders
]

{ #category : #private }
SpTreePresenter >> itemAt: index of: anArray then: path [
	"dives into elements of tree to find the one that corresponds to path"
	| element |
	
	element := anArray at: index.
	^ path 
		ifNotEmpty: [ 
			self 
				itemAt: path first 
				of: (self childrenFor: element) 
				then: path allButFirst ]
		ifEmpty: [ 
			element ]
]

{ #category : #private }
SpTreePresenter >> itemAtPath: anArray [

	^ self 
		itemAt: anArray first
		of: self roots
		then: anArray allButFirst
]

{ #category : #private }
SpTreePresenter >> itemAtPath: anArray ifAbsent: aBlock [
	^ [ self itemAtPath: anArray ]
		on: SubscriptOutOfBounds
		do: [ aBlock value ]
]

{ #category : #api }
SpTreePresenter >> items: aCollection [
	"Set the roots of a tree. This is a convenience method, synonym of `SpTreePresenter>>#roots:`"

	self roots: aCollection
]

{ #category : #initialization }
SpTreePresenter >> registerEvents [

	self whenMenuChangedDo: [ 
		self withAdapterDo: [ :anAdapter | anAdapter updateMenu ] ]
]

{ #category : #api }
SpTreePresenter >> roots [
	"Answer the roots of the tree"

	^ roots
]

{ #category : #api }
SpTreePresenter >> roots: aCollection [
	"Set the roots of the tree table.
	 This is the starting point from where the whole tree will be shown."

	roots := aCollection.
	self selection clearSelection
]

{ #category : #private }
SpTreePresenter >> searchValueOf: item [ 

	^ item asString
]

{ #category : #'api-selection' }
SpTreePresenter >> selectItem: anItem [
	"Select `anItem` if it is included in model list.
	 It does not scrolls to selected element."

	self selection selectItem: anItem
]

{ #category : #'api-selection' }
SpTreePresenter >> selectItems: aListOfItem [
	"Select items included in `aCollection` if they are included in model list.
	 NOTE: In single selection mode it will select the first element of `aCollection` 
	 It does not scrolls to selected element."

	self selection selectItems: aListOfItem
]

{ #category : #'api-selection' }
SpTreePresenter >> selectPath: aPath [
	"Selects element in `aPath`
	`aPath` is the path to select. A path is an array of node indexes (e.g. #(1 2 3)).
	 It does not scrolls to selected element."

	self selection selectPath: aPath
]

{ #category : #'api-selection' }
SpTreePresenter >> selectPath: aPath scrollToSelection: shouldScrollToSelection [
	"Selects element in `aPath`
	`aPath` is the path to select. A path is an array of node indexes (e.g. #(1 2 3)).
	 If `shouldScrollToSelection` is true, it will scroll to selected element."

	self selectPath: aPath.
	shouldScrollToSelection ifTrue: [ adapter scrollToSelection ]
]

{ #category : #'api-selection' }
SpTreePresenter >> selectPaths: pathArray [
	"Selects all elements in `pathsArray``
	`pathsArray` is an array of paths. A path is an array of node indexes (e.g. #(1 2 3))"

	self selection selectPaths: pathArray
]

{ #category : #'api-selection' }
SpTreePresenter >> selectedItem [
	"Return selected item."
	
	^ self selection selectedItem
]

{ #category : #'api-selection' }
SpTreePresenter >> selectedItems [
	"Return all the selected items in the case of a multiple selection list"
	
	^ self selection selectedItems
]

{ #category : #'api-selection' }
SpTreePresenter >> selection [
	"Answer the selection object (an instance of `SpSingleSelectionMode` or `SpMultipleSelectionMode`).
	 This is not the item selected, but the selection container (it may contain one or many selected 
	 items)"
	
	^ selectionMode value
]

{ #category : #private }
SpTreePresenter >> selectionMode: aMode [
	
	selectionMode ifNotNil: [ 
		selectionMode transferSubscriptionsTo: aMode ].
	selectionMode := aMode
]

{ #category : #private }
SpTreePresenter >> shouldLazilyComputeChildren [

	^ false
]

{ #category : #api }
SpTreePresenter >> showColumnHeaders [
	"Show column headers"

	showColumnHeaders := true
]

{ #category : #'api-selection' }
SpTreePresenter >> unselectAll [ 
	"Remove all selections"

	self selection unselectAll
]

{ #category : #'api-selection' }
SpTreePresenter >> unselectItem: anItem [
	"Remove selection of element `anItem`"

	self selection unselectItem: anItem
]

{ #category : #'api-selection' }
SpTreePresenter >> unselectPath: aPath [
	"Unselects element in `aPath`
	`aPath` is the path to select. A path is an array of node indexes (e.g. #(1 2 3))"

	self selection unselectPath: aPath
]

{ #category : #private }
SpTreePresenter >> verticalAlignment [
	^ verticalAlignment
]

{ #category : #'api-events' }
SpTreePresenter >> whenActivatedDo: aBlock [
	"Inform when an element has been 'activated'. 
	 `aBlock` receives one argument (a selection object, see `SpAbstractSelectionMode`)"

	activationBlock := aBlock
]

{ #category : #'api-events' }
SpTreePresenter >> whenMultiSelectionChangedDo: aBlockClosure [ 
	"Inform when selection mode has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"
	
	selectionMode whenChangedDo: aBlockClosure 
]

{ #category : #'api-events' }
SpTreePresenter >> whenRootsChangedDo: aBlockClosure [ 
	"Inform when roots have changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #roots whenChangedDo: aBlockClosure
]

{ #category : #'api-events' }
SpTreePresenter >> whenSelectedIndexChangedDo: aBlock [ 
	"Inform when selected index has changed. 
	 `aBlock` receives one optional argument (the new element)."
	
	self whenSelectionChangedDo: [ :selection | 
		aBlock cull: selection first ]
]

{ #category : #'api-events' }
SpTreePresenter >> whenSelectedItemChangedDo: aBlockClosure [ 
	"Inform when selected index has changed. 
	 `aBlock` receives one optional argument (the new element)."
	
	self whenSelectionChangedDo: [ :selection | aBlockClosure cull: selection second ]
]

{ #category : #'api-events' }
SpTreePresenter >> whenSelectionChangedDo: aBlock [ 
	"Inform when selection has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"
	
	self selection whenChangedDo: [ aBlock cull: selectionMode ]
]

{ #category : #'api-events' }
SpTreePresenter >> whenShowColumnHeadersChangedDo: aBlock [ 
	"Inform when showColumnHeaders property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"
	
	self property: #showColumnHeaders whenChangedDo: aBlock 
]
