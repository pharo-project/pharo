"
A TreeTablePresenter handles a tree table.
Tables defines columns (see #addColumn: and TableColumn hierarchy).
Tables receive items (see #items:) to feed the table.

self example

"
Class {
	#name : #SpTreeTablePresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#traits : 'TSpHaveWrappingScrollBars',
	#classTraits : 'TSpHaveWrappingScrollBars classTrait',
	#instVars : [
		'#columns',
		'#showColumnHeaders => SpObservableSlot',
		'#isResizable => SpObservableSlot',
		'#selection => SpObservableSlot',
		'#doubleClickAction => SpObservableSlot',
		'#roots => SpObservableSlot',
		'#childrenBlock',
		'#contextMenuHolder'
	],
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #specs }
SpTreeTablePresenter class >> adapterName [

	^ #TreeTableAdapter
]

{ #category : #api }
SpTreeTablePresenter >> addColumn: aColumn [

	columns value: (columns value copyWith: aColumn)
]

{ #category : #'selection mode' }
SpTreeTablePresenter >> beMultipleSelection [
	| newSelectionMode |
	
	newSelectionMode := (SpTreeMultipleSelectionMode on: self)
		copyFrom: selection;
		yourself.
	
	selection := newSelectionMode.
]

{ #category : #api }
SpTreeTablePresenter >> beNotResizable [
	self isResizable: false
]

{ #category : #api }
SpTreeTablePresenter >> beResizable [
	self isResizable: true
]

{ #category : #private }
SpTreeTablePresenter >> children [

	^ childrenBlock
]

{ #category : #accessing }
SpTreeTablePresenter >> children: aBlock [

	childrenBlock := aBlock
]

{ #category : #accessing }
SpTreeTablePresenter >> childrenFor: anObject [ 

	^ self children value: anObject
]

{ #category : #accessing }
SpTreeTablePresenter >> columns [

	^ columns value
]

{ #category : #api }
SpTreeTablePresenter >> columns: aCollection [

	columns value: aCollection
]

{ #category : #api }
SpTreeTablePresenter >> contextMenu [
	^ contextMenuHolder value
]

{ #category : #api }
SpTreeTablePresenter >> contextMenu: aValuable [
	"Set the block used to defined the menu"
	
	contextMenuHolder value: aValuable
]

{ #category : #private }
SpTreeTablePresenter >> doubleClickAction [

	^ doubleClickAction
]

{ #category : #'api-events' }
SpTreeTablePresenter >> doubleClickAction: aBlockClosure [ 
	
	doubleClickAction := aBlockClosure
]

{ #category : #simulation }
SpTreeTablePresenter >> doubleClickAtIndex: anInteger [ 
	
	self selectedIndex: anInteger.
	doubleClickAction cull: anInteger
]

{ #category : #api }
SpTreeTablePresenter >> hideColumnHeaders [

	showColumnHeaders := false
]

{ #category : #initialization }
SpTreeTablePresenter >> initialize [

	super initialize.

	self withScrollBars.	
	showColumnHeaders := true.
	columns := #() asValueHolder.
	isResizable := false.
	selection := SpTreeSingleSelectionMode on: self.
	childrenBlock :=  [ :item | #() ].
	roots := #().
	contextMenuHolder := nil asValueHolder.
	self registerEvents 
]

{ #category : #accessing }
SpTreeTablePresenter >> isResizable [
	^ isResizable
]

{ #category : #accessing }
SpTreeTablePresenter >> isResizable: aBoolean [
	isResizable := aBoolean
]

{ #category : #testing }
SpTreeTablePresenter >> isShowingColumnHeaders [

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

{ #category : #accessing }
SpTreeTablePresenter >> itemAtPath: anArray [

	^ self 
		itemAt: anArray first
		of: self roots
		then: anArray allButFirst
]

{ #category : #accessing }
SpTreeTablePresenter >> itemAtPath: anArray ifAbsent: aBlock [
	^ [ self itemAtPath: anArray ]
		on: SubscriptOutOfBounds
		do: [ aBlock value ]
]

{ #category : #initialization }
SpTreeTablePresenter >> registerEvents [
	contextMenuHolder whenChangedDo: [ self withAdapterDo: #updateMenu ]
]

{ #category : #accessing }
SpTreeTablePresenter >> roots [

	^ roots
]

{ #category : #accessing }
SpTreeTablePresenter >> roots: aCollection [

	roots := aCollection.
	self selection clearSelection
]

{ #category : #api }
SpTreeTablePresenter >> selectPath: aPath [

	self selection selectPath: aPath
]

{ #category : #api }
SpTreeTablePresenter >> selectPaths: pathArray [
	self selection selectPaths: pathArray
]

{ #category : #api }
SpTreeTablePresenter >> selectedItem [
	
	^ self selection selectedItem
]

{ #category : #accessing }
SpTreeTablePresenter >> selection [
	
	^ selection value
]

{ #category : #api }
SpTreeTablePresenter >> showColumnHeaders [

	showColumnHeaders := true
]

{ #category : #api }
SpTreeTablePresenter >> unselectAll [ 

	self selection unselectAll
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenColumnsChangedDo: aBlockClosure [ 
	
	columns whenChangedDo: aBlockClosure 
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenIsResizableChangedDo: aBlock [

	self property: #isResizable whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenMenuChangedDo: aBlock [
	"Set a block to value when the menu block has changed"
	
	contextMenuHolder whenChangedDo: aBlock
]

{ #category : #accessing }
SpTreeTablePresenter >> whenMultiSelectionChangedDo: aBlockClosure [ 
	
	selection whenChangedDo: aBlockClosure 
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenRootsChangedDo: aBlockClosure [ 

	self property: #roots whenChangedDo: aBlockClosure
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenSelectedIndexChangedDo: aBlockClosure [ 
	
	self whenSelectionChangedDo: [ :selection | aBlockClosure cull: selection first ]
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenSelectedItemChangedDo: aBlockClosure [ 
	
	self whenSelectionChangedDo: [ :selection | aBlockClosure cull: selection second ]
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenSelectionChangedDo: aBlock [ 
	
	self selection whenChangedDo: [ aBlock cull: selection ]
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenShowColumnHeadersChangedDo: aBlockClosure [ 
	
	self property: #showColumnHeaders whenChangedDo: aBlockClosure 
]
