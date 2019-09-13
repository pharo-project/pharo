"
A TreeTablePresenter handles a tree table.
Tables defines columns (see #addColumn: and TableColumn hierarchy).
Tables receive items (see #items:) to feed the table.

self example

"
Class {
	#name : #SpTreeTablePresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#traits : 'TSpHaveWrappingScrollBars + SpTContextMenu',
	#classTraits : 'TSpHaveWrappingScrollBars classTrait + SpTContextMenu classTrait',
	#instVars : [
		'#columns => SpObservableSlot',
		'#showColumnHeaders => SpObservableSlot',
		'#isResizable => SpObservableSlot',
		'#selection => SpObservableSlot',
		'#roots => SpObservableSlot',
		'#childrenBlock',
		'#activationBlock',
		'#activateOnSingleClick'
	],
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #specs }
SpTreeTablePresenter class >> adapterName [

	^ #TreeTableAdapter
]

{ #category : #api }
SpTreeTablePresenter >> activateOnDoubleClick [
	
	activateOnSingleClick := false
]

{ #category : #api }
SpTreeTablePresenter >> activateOnSingleClick [
	
	activateOnSingleClick := true
]

{ #category : #testing }
SpTreeTablePresenter >> activatesOnDoubleClick [
	
	^ activateOnSingleClick not
]

{ #category : #testing }
SpTreeTablePresenter >> activatesOnSingleClick [
	
	^ activateOnSingleClick
]

{ #category : #api }
SpTreeTablePresenter >> addColumn: aColumn [
	columns := self columns copyWith: aColumn
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

{ #category : #simulation }
SpTreeTablePresenter >> clickAtPath: aPath [

	self selectPath: aPath.

	activateOnSingleClick ifFalse: [ ^ self ].
	self doActivateAtPath: aPath
]

{ #category : #accessing }
SpTreeTablePresenter >> columns [
	^ columns
]

{ #category : #api }
SpTreeTablePresenter >> columns: aCollection [
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
SpTreeTablePresenter >> hideColumnHeaders [

	showColumnHeaders := false
]

{ #category : #initialization }
SpTreeTablePresenter >> initialize [
	super initialize.

	self withScrollBars.
	
	activationBlock := [ ].
	
	showColumnHeaders := true.
	columns := #().
	isResizable := false.
	selection := SpTreeSingleSelectionMode on: self.
	childrenBlock := [ :item | #() ].
	roots := #().
	self activateOnDoubleClick.
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

	self whenMenuChangedDo: [ 
		self withAdapterDo: [ :anAdapter | anAdapter updateMenu ] ]
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
SpTreeTablePresenter >> whenActivatedDo: aBlockClosure [
	activationBlock := aBlockClosure
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenColumnsChangedDo: aBlockClosure [
	self property: #columns whenChangedDo: aBlockClosure
]

{ #category : #'api-events' }
SpTreeTablePresenter >> whenIsResizableChangedDo: aBlock [

	self property: #isResizable whenChangedDo: aBlock
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
