"
A presenter for tree with a table.
A table has columns with a type (See column types section).

**IMPORTANT:** A table presenter does not contain other presenters. Instead, it maps the model provided (See `SpTreeTablePresenter>>#roots:` and `SpTreeTablePresenter>>#children:`) with the columns defined. If you need this behavior, you will need to use `SpComponentListPresenter`.

"
Class {
	#name : #SpTreeTablePresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#traits : 'SpTHaveWrappingScrollBars + SpTContextMenu + SpTSearchable',
	#classTraits : 'SpTHaveWrappingScrollBars classTrait + SpTContextMenu classTrait + SpTSearchable classTrait',
	#instVars : [
		'#columns => SpObservableSlot',
		'#showColumnHeaders => SpObservableSlot',
		'#isResizable => SpObservableSlot',
		'#roots => SpObservableSlot',
		'#selectionMode',
		'#childrenBlock',
		'#activationBlock',
		'#activateOnSingleClick',
		'#lazilyComputeChildren',
		'#verticalAlignment'
	],
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #specs }
SpTreeTablePresenter class >> adapterName [

	^ #TreeTableAdapter
]

{ #category : #documentation }
SpTreeTablePresenter class >> addDocumentExtraSections: aBuilder [

	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Column types' ] withLevel: 2.
	aBuilder unorderedListDuring: [ 
		(SpTableColumn allSubclasses sorted: #name ascending) do: [ :eachType | 
			aBuilder item: [
				aBuilder monospace: eachType name ] ] ]
]

{ #category : #documentation }
SpTreeTablePresenter class >> documentFactoryMethodSelector [

	^ #newTreeTable
]

{ #category : #documentation }
SpTreeTablePresenter class >> documentSections [
		
	^ OrderedDictionary newFromPairs: {
		'Examples'. self class methods select: [ :each | each protocol = self documentExamplesProtocol ].
		'API Methods'. self methods select: [ :each | each protocol = #'api' ].
		'API Selection Methods'. self methods select: [ :each | each protocol = #'api-selection' ].
		'Testing Methods'. self methods select: [ :each | each protocol = #'testing' ].
		'Events'. self methods select: [ :each | each protocol = #'api-events' ].
	 }
]

{ #category : #api }
SpTreeTablePresenter >> activateOnDoubleClick [
	"Configure the list to trigger activation on double click.
	 An element on a list can be 'activated', meaning it will trigger an event to execute an 
	 action on it. An activation is different than a selection (one can 'select' an element without 
	 activating it).
	 See `SpTreeTablePresenter>>#whenActivatedDo:`"
		
	activateOnSingleClick := false
]

{ #category : #api }
SpTreeTablePresenter >> activateOnSingleClick [
	"Configure the list to trigger activation on single click.
	 An element on a list can be 'activated', meaning it will trigger an event to execute an 
	 action on it. An activation is different than a selection (one can 'select' an element without 
	 activating it).
	 See `SpTreeTablePresenter>>#whenActivatedDo:`"
	
	activateOnSingleClick := true
]

{ #category : #testing }
SpTreeTablePresenter >> activatesOnDoubleClick [
	"Answer true if activation event is triggered on double click"
	
	^ activateOnSingleClick not
]

{ #category : #testing }
SpTreeTablePresenter >> activatesOnSingleClick [
	"Answer true if activation event is triggered on single click"
	
	^ activateOnSingleClick
]

{ #category : #api }
SpTreeTablePresenter >> addColumn: aColumn [
	"Add a column to the table. A column should be an instance of `SpTableColumn`"

	columns := self columns copyWith: aColumn
]

{ #category : #api }
SpTreeTablePresenter >> beMultipleSelection [
	"Enable multiple selection."

	self selectionMode: (SpTreeMultipleSelectionMode on: self)
]

{ #category : #api }
SpTreeTablePresenter >> beNotResizable [
	"Mark the table as 'not resizable', which means there will be not possibility to resize the 
	 columns of it."

	self isResizable: false
]

{ #category : #api }
SpTreeTablePresenter >> beResizable [
	"Mark the table as 'resizable', which means there will be a slider to resize the columns."

	self isResizable: true
]

{ #category : #api }
SpTreeTablePresenter >> beSingleSelection [
	"Enable single selection (this is the default)."
	
	self selectionMode: (SpTreeSingleSelectionMode on: self)
]

{ #category : #private }
SpTreeTablePresenter >> children [

	^ childrenBlock
]

{ #category : #api }
SpTreeTablePresenter >> children: aBlock [
	"Set a block to answer the children of a node when it is expanded.
	 `aBlock` receives one argument, the node element to expand.
	 If there are no children to answer, `aBlock` needs to answer an empty collection."

	childrenBlock := aBlock
]

{ #category : #private }
SpTreeTablePresenter >> childrenFor: anObject [ 

	^ self children value: anObject
]

{ #category : #simulation }
SpTreeTablePresenter >> clickAtPath: aPath [

	self selectPath: aPath.

	activateOnSingleClick ifFalse: [ ^ self ].
	self doActivateAtPath: aPath
]

{ #category : #api }
SpTreeTablePresenter >> columns [
	"Answer the columns composing this table."

	^ columns
]

{ #category : #api }
SpTreeTablePresenter >> columns: aCollection [
	"Set all columns at once. 
	 `aCollection` is a list of instances of `SpTableColumn`"

	columns := aCollection
]

{ #category : #simulation }
SpTreeTablePresenter >> doActivateAtPath: aPath [
	"Activate only if there is an item at that position"

	self itemAtPath: aPath ifAbsent: [ ^ self ].
	activationBlock
		cull:
			((SpTreeSingleSelectionMode on: self)
				selectPath: aPath;
				yourself)
]

{ #category : #simulation }
SpTreeTablePresenter >> doubleClickAtPath: aPath [
	self selectPath: aPath.
	
	activateOnSingleClick ifTrue: [ ^ self ].
	self doActivateAtPath: aPath
]

{ #category : #api }
SpTreeTablePresenter >> expandPath: aPath [
	"Expand the tree path.
	`aPath` is the path to expand. A path is an array of node indexes (e.g. #(1 2 3))"

	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter expandPath: aPath ]
]

{ #category : #api }
SpTreeTablePresenter >> expandRoots [
	"Expand all roots of the tree"

	self withAdapterPerformOrDefer: [ :anAdapter | 
		anAdapter expandRoots ]
]

{ #category : #api }
SpTreeTablePresenter >> hideColumnHeaders [
	"Hide the column headers"

	showColumnHeaders := false
]

{ #category : #initialization }
SpTreeTablePresenter >> initialize [
	super initialize.
	self initializeTSearchable.

	self withScrollBars.
	
	activationBlock := [ ].
	
	showColumnHeaders := true.
	columns := #().
	isResizable := false.
	childrenBlock := [ :item | #() ].
	roots := #().
	lazilyComputeChildren := false.
	verticalAlignment := SpVerticalAlignment new.


	self beSingleSelection.
	self activateOnDoubleClick.
	self registerEvents
]

{ #category : #testing }
SpTreeTablePresenter >> isResizable [
	"Answer true if table allows resizing of its columns."

	^ isResizable
]

{ #category : #private }
SpTreeTablePresenter >> isResizable: aBoolean [
	isResizable := aBoolean
]

{ #category : #testing }
SpTreeTablePresenter >> isShowingColumnHeaders [
	"Answer true if the table is configured to show column headers."

	^ showColumnHeaders
]

{ #category : #private }
SpTreeTablePresenter >> itemAt: index of: anArray then: path [
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
SpTreeTablePresenter >> itemAtPath: anArray [

	^ self 
		itemAt: anArray first
		of: self roots
		then: anArray allButFirst
]

{ #category : #private }
SpTreeTablePresenter >> itemAtPath: anArray ifAbsent: aBlock [
	^ [ self itemAtPath: anArray ]
		on: SubscriptOutOfBounds
		do: [ aBlock value ]
]

{ #category : #api }
SpTreeTablePresenter >> items: aCollection [
	"Set the roots of a tree. This is a convenience method, synonym of `SpTreeTablePresenter>>#roots:`"

	self roots: aCollection
]

{ #category : #private }
SpTreeTablePresenter >> lazilyComputeChildren [
	"When we compute lazily the children, we will show in all cases the expand arrow even if there is no children until we open this children."

	lazilyComputeChildren := true
]

{ #category : #private }
SpTreeTablePresenter >> lazilyComputeChildren: aBoolean [
	lazilyComputeChildren := aBoolean
]

{ #category : #initialization }
SpTreeTablePresenter >> registerEvents [

	self whenMenuChangedDo: [ 
		self withAdapterDo: [ :anAdapter | anAdapter updateMenu ] ]
]

{ #category : #api }
SpTreeTablePresenter >> roots [
	"Answer the roots of the tree"

	^ roots
]

{ #category : #api }
SpTreeTablePresenter >> roots: aCollection [
	"Set the roots of the tree table.
	 This is the starting point from where the whole tree will be shown."

	roots := aCollection.
	self selection clearSelection
]

{ #category : #private }
SpTreeTablePresenter >> searchValueOf: item [ 

	^ item asString
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> selectItem: anItem [
	"Select `anItem` if it is included in model list.
	 It does not scrolls to selected element."
	
	self selection selectItem: anItem
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> selectItems: aListOfItem [
	"Select items included in `aCollection` if they are included in model list.
	 NOTE: In single selection mode it will select the first element of `aCollection` 
	 It does not scrolls to selected element."

	self selection selectItems: aListOfItem
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> selectPath: aPath [
	"Selects element in `aPath`
	`aPath` is the path to select. A path is an array of node indexes (e.g. #(1 2 3)).
	 It does not scrolls to selected element."

	self selection selectPath: aPath
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> selectPath: aPath scrollToSelection: shouldScrollToSelection [
	"Selects element in `aPath`
	`aPath` is the path to select. A path is an array of node indexes (e.g. #(1 2 3)).
	 If `shouldScrollToSelection` is true, it will scroll to selected element."

	self selectPath: aPath.
	shouldScrollToSelection ifTrue: [ adapter scrollToSelection ]
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> selectPaths: pathsArray [
	"Selects all elements in `pathsArray``
	`pathsArray` is an array of paths. A path is an array of node indexes (e.g. #(1 2 3))"

	self selection selectPaths: pathsArray
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> selectedItem [
	"Return selected item."
	
	^ self selection selectedItem
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> selectedItems [
	"Return all the selected items in the case of a multiple selection list"
	
	^ self selection selectedItems
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> selection [
	"Answer the selection object (an instance of `SpSingleSelectionMode` or `SpMultipleSelectionMode`).
	 This is not the item selected, but the selection container (it may contain one or many selected 
	 items)"
	
	^ selectionMode value
]

{ #category : #private }
SpTreeTablePresenter >> selectionMode: aMode [
	
	selectionMode ifNotNil: [ 
		selectionMode transferSubscriptionsTo: aMode ].
	selectionMode := aMode
]

{ #category : #private }
SpTreeTablePresenter >> shouldLazilyComputeChildren [
	^ lazilyComputeChildren
]

{ #category : #api }
SpTreeTablePresenter >> showColumnHeaders [
	"Show column headers"

	showColumnHeaders := true
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> unselectAll [ 
	"Remove all selections"

	self selection unselectAll
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> unselectItem: anItem [
	"Remove selection of element `anItem`"

	self selection unselectItem: anItem
]

{ #category : #'api-selection' }
SpTreeTablePresenter >> unselectPath: aPath [
	"Unselects element in `aPath`
	`aPath` is the path to select. A path is an array of node indexes (e.g. #(1 2 3))"

	self selection unselectPath: aPath
]

{ #category : #private }
SpTreeTablePresenter >> verticalAlignment [
	^ verticalAlignment
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenActivatedDo: aBlock [
	"Inform when an element has been 'activated'. 
	 `aBlock` receives one argument (a selection object, see `SpAbstractSelectionMode`)"
	
	activationBlock := aBlock
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenColumnsChangedDo: aBlock [
	"Inform when columns have changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"	
	

	self property: #columns whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenIsResizableChangedDo: aBlock [
	"Inform when resizable property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #isResizable whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenMultiSelectionChangedDo: aBlock [
	"Inform when selection mode has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"
	
	selectionMode whenChangedDo: aBlock 
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenRootsChangedDo: aBlock [ 
	"Inform when roots have changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #roots whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenSelectedIndexChangedDo: aBlock [ 
	"Inform when selected index has changed. 
	 `aBlock` receives one optional argument (the new element)."
	
	self whenSelectionChangedDo: [ :selection | 
		aBlock cull: selection first ]
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenSelectedItemChangedDo: aBlock [
	"Inform when selected index has changed. 
	 `aBlock` receives one optional argument (the new element)."
	
	self whenSelectionChangedDo: [ :selection | 
		aBlock cull: (selection ifNotNil: [ :anArray | anArray second ]) ]
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenSelectionChangedDo: aBlock [ 
	"Inform when selection has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"
	
	self selection whenChangedDo: [ aBlock cull: selectionMode ]
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenShowColumnHeadersChangedDo: aBlock [ 
	"Inform when showColumnHeaders property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"
	
	self property: #showColumnHeaders whenChangedDo: aBlock 
]
