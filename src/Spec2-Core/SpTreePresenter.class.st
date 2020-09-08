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

{ #category : #api }
SpTreePresenter >> activateOnDoubleClick [
	
	activateOnSingleClick := false
]

{ #category : #api }
SpTreePresenter >> activateOnSingleClick [
	
	activateOnSingleClick := true
]

{ #category : #testing }
SpTreePresenter >> activatesOnDoubleClick [
	
	^ activateOnSingleClick not
]

{ #category : #testing }
SpTreePresenter >> activatesOnSingleClick [
	
	^ activateOnSingleClick
]

{ #category : #api }
SpTreePresenter >> beMultipleSelection [

	self selectionMode: (SpTreeMultipleSelectionMode on: self)
]

{ #category : #api }
SpTreePresenter >> beNotResizable [
	self isResizable: false
]

{ #category : #api }
SpTreePresenter >> beResizable [
	self isResizable: true
]

{ #category : #api }
SpTreePresenter >> beSingleSelection [
	
	self selectionMode: (SpTreeSingleSelectionMode on: self)
]

{ #category : #private }
SpTreePresenter >> children [

	^ childrenBlock
]

{ #category : #accessing }
SpTreePresenter >> children: aBlock [

	childrenBlock := aBlock
]

{ #category : #accessing }
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

	^ displayBlock
]

{ #category : #api }
SpTreePresenter >> display: aBlock [

	displayBlock := aBlock
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
SpTreePresenter >> hideColumnHeaders [

	showColumnHeaders := false
]

{ #category : #api }
SpTreePresenter >> icon [
	
	^ iconBlock
]

{ #category : #api }
SpTreePresenter >> icon: aBlock [

	iconBlock := aBlock
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
	^ isResizable
]

{ #category : #private }
SpTreePresenter >> isResizable: aBoolean [
	isResizable := aBoolean
]

{ #category : #testing }
SpTreePresenter >> isShowingColumnHeaders [

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

{ #category : #accessing }
SpTreePresenter >> itemAtPath: anArray [

	^ self 
		itemAt: anArray first
		of: self roots
		then: anArray allButFirst
]

{ #category : #accessing }
SpTreePresenter >> itemAtPath: anArray ifAbsent: aBlock [
	^ [ self itemAtPath: anArray ]
		on: SubscriptOutOfBounds
		do: [ aBlock value ]
]

{ #category : #api }
SpTreePresenter >> items: aCollection [
	"Set the roots of a tree. This is a convenience method, synonym of `SpTreeTablePresenter>>#roots:`"

	self roots: aCollection
]

{ #category : #initialization }
SpTreePresenter >> registerEvents [

	self whenMenuChangedDo: [ 
		self withAdapterDo: [ :anAdapter | anAdapter updateMenu ] ]
]

{ #category : #accessing }
SpTreePresenter >> roots [

	^ roots
]

{ #category : #accessing }
SpTreePresenter >> roots: aCollection [

	roots := aCollection.
	self selection clearSelection
]

{ #category : #private }
SpTreePresenter >> searchValueOf: item [ 

	^ item asString
]

{ #category : #api }
SpTreePresenter >> selectItem: anItem [

	self selection selectItem: anItem
]

{ #category : #api }
SpTreePresenter >> selectItems: aListOfItem [

	self selection selectItems: aListOfItem
]

{ #category : #api }
SpTreePresenter >> selectPath: aPath [

	self selection selectPath: aPath
]

{ #category : #api }
SpTreePresenter >> selectPath: aPath scrollToSelection: shouldScrollToSelection [

	self selectPath: aPath.
	shouldScrollToSelection ifTrue: [ adapter scrollToSelection ]
]

{ #category : #api }
SpTreePresenter >> selectPaths: pathArray [
	self selection selectPaths: pathArray
]

{ #category : #api }
SpTreePresenter >> selectedItem [
	
	^ self selection selectedItem
]

{ #category : #api }
SpTreePresenter >> selectedItems [
	
	^ self selection selectedItems
]

{ #category : #accessing }
SpTreePresenter >> selection [
	
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

	showColumnHeaders := true
]

{ #category : #api }
SpTreePresenter >> unselectAll [ 

	self selection unselectAll
]

{ #category : #api }
SpTreePresenter >> unselectItem: anItem [

	self selection unselectItem: anItem
]

{ #category : #api }
SpTreePresenter >> unselectPath: aPath [

	self selection unselectPath: aPath
]

{ #category : #accessing }
SpTreePresenter >> verticalAlignment [
	^ verticalAlignment
]

{ #category : #'api-events' }
SpTreePresenter >> whenActivatedDo: aBlock [

	activationBlock := aBlock
]

{ #category : #accessing }
SpTreePresenter >> whenMultiSelectionChangedDo: aBlockClosure [ 
	
	selectionMode whenChangedDo: aBlockClosure 
]

{ #category : #'api-events' }
SpTreePresenter >> whenRootsChangedDo: aBlockClosure [ 

	self property: #roots whenChangedDo: aBlockClosure
]

{ #category : #'api-events' }
SpTreePresenter >> whenSelectedIndexChangedDo: aBlockClosure [ 
	
	self whenSelectionChangedDo: [ :selection | aBlockClosure cull: selection first ]
]

{ #category : #'api-events' }
SpTreePresenter >> whenSelectedItemChangedDo: aBlockClosure [ 
	
	self whenSelectionChangedDo: [ :selection | aBlockClosure cull: selection second ]
]

{ #category : #'api-events' }
SpTreePresenter >> whenSelectionChangedDo: aBlock [ 
	
	self selection whenChangedDo: [ aBlock cull: selectionMode ]
]

{ #category : #'api-events' }
SpTreePresenter >> whenShowColumnHeadersChangedDo: aBlockClosure [ 
	
	self property: #showColumnHeaders whenChangedDo: aBlockClosure 
]
