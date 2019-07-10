"
A TreeTablePresenter handles a tree table.
Tables defines columns (see #addColumn: and TableColumn hierarchy).
Tables receive items (see #items:) to feed the table.

self example

"
Class {
	#name : #TreeTablePresenter,
	#superclass : #AbstractWidgetPresenter,
	#traits : 'TSpecObservable + TSpecHaveWrappingScrollBars',
	#classTraits : 'TSpecObservable classTrait + TSpecHaveWrappingScrollBars classTrait',
	#instVars : [
		'#columns',
		'#showColumnHeaders => SpecObservableSlot',
		'#isResizable => SpecObservableSlot',
		'#selection => SpecObservableSlot',
		'#doubleClickAction => SpecObservableSlot',
		'#roots => SpecObservableSlot',
		'#childrenBlock',
		'#contextMenuHolder'
	],
	#category : #'Spec-Core-Widgets-Tree'
}

{ #category : #specs }
TreeTablePresenter class >> adapterName [

	^ #TreeTableAdapter
]

{ #category : #api }
TreeTablePresenter >> addColumn: aColumn [

	columns value: (columns value copyWith: aColumn)
]

{ #category : #'selection mode' }
TreeTablePresenter >> beMultipleSelection [
	| newSelectionMode |
	
	newSelectionMode := (TreeMultipleSelectionMode on: self)
		copyFrom: selection;
		yourself.
	
	selection := newSelectionMode.
]

{ #category : #api }
TreeTablePresenter >> beNotResizable [
	self isResizable: false
]

{ #category : #api }
TreeTablePresenter >> beResizable [
	self isResizable: true
]

{ #category : #private }
TreeTablePresenter >> children [

	^ childrenBlock
]

{ #category : #accessing }
TreeTablePresenter >> children: aBlock [

	childrenBlock := aBlock
]

{ #category : #accessing }
TreeTablePresenter >> childrenFor: anObject [ 

	^ self children value: anObject
]

{ #category : #accessing }
TreeTablePresenter >> columns [

	^ columns value
]

{ #category : #api }
TreeTablePresenter >> columns: aCollection [

	columns value: aCollection
]

{ #category : #api }
TreeTablePresenter >> contextMenu [
	^ contextMenuHolder value
]

{ #category : #api }
TreeTablePresenter >> contextMenu: aValuable [
	"Set the block used to defined the menu"
	
	contextMenuHolder value: aValuable
]

{ #category : #private }
TreeTablePresenter >> doubleClickAction [

	^ doubleClickAction
]

{ #category : #'api-events' }
TreeTablePresenter >> doubleClickAction: aBlockClosure [ 
	
	doubleClickAction := aBlockClosure
]

{ #category : #simulation }
TreeTablePresenter >> doubleClickAtIndex: anInteger [ 
	
	self selectedIndex: anInteger.
	doubleClickAction cull: anInteger
]

{ #category : #api }
TreeTablePresenter >> hideColumnHeaders [

	showColumnHeaders := false
]

{ #category : #initialization }
TreeTablePresenter >> initialize [

	super initialize.

	self withScrollBars.	
	showColumnHeaders := true.
	columns := #() asValueHolder.
	isResizable := false.
	selection := TreeSingleSelectionMode on: self.
	childrenBlock :=  [ :item | #() ].
	roots := #().
	contextMenuHolder := nil asValueHolder.
	self registerEvents 
]

{ #category : #accessing }
TreeTablePresenter >> isResizable [
	^ isResizable
]

{ #category : #accessing }
TreeTablePresenter >> isResizable: aBoolean [
	isResizable := aBoolean
]

{ #category : #testing }
TreeTablePresenter >> isShowingColumnHeaders [

	^ showColumnHeaders
]

{ #category : #private }
TreeTablePresenter >> itemAt: index of: anArray then: path [
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
TreeTablePresenter >> itemAtPath: anArray [

	^ self 
		itemAt: anArray first
		of: self roots
		then: anArray allButFirst
]

{ #category : #accessing }
TreeTablePresenter >> itemAtPath: anArray ifAbsent: aBlock [
	^ [ self itemAtPath: anArray ]
		on: SubscriptOutOfBounds
		do: [ aBlock value ]
]

{ #category : #initialization }
TreeTablePresenter >> registerEvents [
	contextMenuHolder whenChangedDo: [ self withAdapterDo: #updateMenu ]
]

{ #category : #accessing }
TreeTablePresenter >> roots [

	^ roots
]

{ #category : #accessing }
TreeTablePresenter >> roots: aCollection [

	roots := aCollection.
	self selection clearSelection
]

{ #category : #api }
TreeTablePresenter >> selectPath: aPath [

	self selection selectPath: aPath
]

{ #category : #api }
TreeTablePresenter >> selectPaths: pathArray [
	self selection selectPaths: pathArray
]

{ #category : #api }
TreeTablePresenter >> selectedItem [
	
	^ self selection selectedItem
]

{ #category : #accessing }
TreeTablePresenter >> selection [
	
	^ selection value
]

{ #category : #api }
TreeTablePresenter >> showColumnHeaders [

	showColumnHeaders := true
]

{ #category : #api }
TreeTablePresenter >> unselectAll [ 

	self selection unselectAll
]

{ #category : #'api-events' }
TreeTablePresenter >> whenColumnsChangedDo: aBlockClosure [ 
	
	columns whenChangedDo: aBlockClosure 
]

{ #category : #'api-events' }
TreeTablePresenter >> whenIsResizableChangedDo: aBlock [

	self property: #isResizable whenChangedDo: aBlock
]

{ #category : #'api-events' }
TreeTablePresenter >> whenMenuChangedDo: aBlock [
	"Set a block to value when the menu block has changed"
	
	contextMenuHolder whenChangedDo: aBlock
]

{ #category : #accessing }
TreeTablePresenter >> whenMultiSelectionChangedDo: aBlockClosure [ 
	
	selection whenChangedDo: aBlockClosure 
]

{ #category : #'api-events' }
TreeTablePresenter >> whenRootsChangedDo: aBlockClosure [ 

	self property: #roots whenChangedDo: aBlockClosure
]

{ #category : #'api-events' }
TreeTablePresenter >> whenSelectedIndexChangedDo: aBlockClosure [ 
	
	self whenSelectionChangedDo: [ :selection | aBlockClosure cull: selection first ]
]

{ #category : #'api-events' }
TreeTablePresenter >> whenSelectedItemChangedDo: aBlockClosure [ 
	
	self whenSelectionChangedDo: [ :selection | aBlockClosure cull: selection second ]
]

{ #category : #'api-events' }
TreeTablePresenter >> whenSelectionChangedDo: aBlock [ 
	
	self selection whenChangedDo: [ aBlock cull: selection ]
]

{ #category : #'api-events' }
TreeTablePresenter >> whenShowColumnHeadersChangedDo: aBlockClosure [ 
	
	self property: #showColumnHeaders whenChangedDo: aBlockClosure 
]
