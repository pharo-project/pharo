"
A DropListPresenter is a Spec presenter for drop lists.

I am assume there is a little problem on an empty list, but frankly, who creates an empty drop list ?

See DropListExample for an example
"
Class {
	#name : #DropListPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'#model => SpecObservableSlot',
		'#startsWithSelection',
		'#selection',
		'#displayBlock => SpecObservableSlot',
		'#iconBlock => SpecObservableSlot'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
DropListPresenter class >> adapterName [

	^ #DropListAdapter
]

{ #category : #specs }
DropListPresenter class >> title [

	^ 'Drop List'
]

{ #category : #api }
DropListPresenter >> addItemLabeled: aString do: aBlock [

	self addItemLabeled: aString do: aBlock icon: nil.
]

{ #category : #api }
DropListPresenter >> addItemLabeled: aString do: aBlock icon: anIcon [

	| item |
	item := DropListItem on: aString do: aBlock.
	item icon: anIcon.

	model add: item.
	"Adding an item adds it to the selection if there is none".
	self selection isEmpty ifTrue: [ 
		self selection selectItem: item ]
]

{ #category : #api }
DropListPresenter >> displayBlock [
	
	^ displayBlock
]

{ #category : #api }
DropListPresenter >> displayBlock: aBlock [
	
	displayBlock := aBlock 

]

{ #category : #api }
DropListPresenter >> displayForItem: anItem [
	"The order of the arguments may look weird, but then it seems more natural while using the widget"
	
	^ self displayBlock cull: anItem model cull: anItem
]

{ #category : #api }
DropListPresenter >> dropListItems: dropListItems [
	"Populate the drop list with a list DropListItems"
	
	model collection: dropListItems
]

{ #category : #api }
DropListPresenter >> emptyList [
	
	model collection: OrderedCollection new.

]

{ #category : #api }
DropListPresenter >> getIconFor: anItem [

	^ self iconBlock cull: anItem model cull: anItem
]

{ #category : #api }
DropListPresenter >> getIndex [

	^ selection selectedIndex
]

{ #category : #api }
DropListPresenter >> getList [

	^ model items
]

{ #category : #testing }
DropListPresenter >> hasIcons [
	
	^ iconBlock isNotNil and: [ self model collection anySatisfy: [ :e | e icon isNotNil ] ]
]

{ #category : #api }
DropListPresenter >> iconBlock [ 
	
	^ iconBlock 
]

{ #category : #api }
DropListPresenter >> iconBlock: aBlock [	
	
	iconBlock := aBlock
]

{ #category : #api }
DropListPresenter >> iconHolder [
	
	self 
		deprecated: 'Use iconBlock instead' 
		transformWith: '`@receiver iconHolder' -> '`@receiver iconBlock'.

	^ self iconBlock
]

{ #category : #api }
DropListPresenter >> iconHolder: aBlock [	
	self 
		deprecated: 'Use iconBlock: instead' 
		transformWith: '`@receiver iconHolder: `@statements1' -> '`@receiver iconBlock: `@statements1'.
	
	self iconBlock: aBlock
]

{ #category : #initialization }
DropListPresenter >> initialize [

	self class initializeSlots: self.
	super initialize.
	
	model := SpecCollectionListModel on: #().
	selection := SpecSingleSelectionMode on: self.
	displayBlock := [ :itemModel :item | item label ].
	iconBlock := [ :itemModel :item | item icon ].
	
	startsWithSelection := true.
		
"	listHolder whenChangedDo: [ 
		self changed: #getList.
		self changed: #getIconFor:.
		self internalResetSelection.].
				
	selectionHolder whenChangedDo: [ self changed: #getIndex ].
	iconHolder := [ Name already defined -> :model :item | item icon ] asValueHolder.
	
	"

]

{ #category : #initialization }
DropListPresenter >> initializePorts [
	self addOutputPort: DropListSelectionPresenterPort new.
	self addInputPort: ItemsPresenterPort new
]

{ #category : #api }
DropListPresenter >> items: aList [
	"Populate the drop list with a list of ui specific items"
	"aList is a list of domain specific objects. If you want to specify more precisely the item actions, see #addItemLabeled:do:"
	"<api: #st getter: #listItems>"
	
	| dropListItems |
	dropListItems := aList collect: [ :e | DropListItem on: e do: [] ].
	model collection: dropListItems.
	
	(dropListItems isNotEmpty and: [startsWithSelection and: [ self selection isEmpty ]]) 
		ifTrue: [ self selectedIndex: 1 ]
]

{ #category : #api }
DropListPresenter >> listItems [
	"<api: #inspect>"
	"Return the list used to populate the drop list"
	
	^ self getList collect: [ :e | e model ].
]

{ #category : #api }
DropListPresenter >> listSize [
	"<api: #inspect>"
	"Return the size of the list of choices"

	^ self listItems size
]

{ #category : #api }
DropListPresenter >> model [
	
	^ model
]

{ #category : #selection }
DropListPresenter >> resetSelection [
	
	self selection unselectAll 
]

{ #category : #api }
DropListPresenter >> selectedIndex [
	"<api: #inspect>"
	"Useless method but it provides a better and more consistent API"

	^ self getIndex
]

{ #category : #API }
DropListPresenter >> selectedIndex: anInteger [ 
	
	self selection selectIndex: anInteger.
	"I execute the block associated with the item"
	self selection selectedItem value.
]

{ #category : #api }
DropListPresenter >> selectedItem [
	"<api: #inspect>"

	^ self selection selectedItem ifNotNil: #model
]

{ #category : #api }
DropListPresenter >> selectedItem: anItem [ 
	
	| realItem |
	
	anItem ifNil: [ self selection unselectAll. ^self ].
	
	realItem := model collection detect: [ :each | each model = anItem ].
	self selection selectItem: realItem.
	"I execute the block associated with the item"
	realItem value.
]

{ #category : #api }
DropListPresenter >> selection [
	^ selection
]

{ #category : #api }
DropListPresenter >> setSelectedIndex: anIndex [
	self
		deprecated: 'Use #selectedIndex: instead'
		transformWith:
			'`@receiver setSelectedIndex: `@argument'
				-> '`@receiver selectedIndex:`@argument'.
	^ self selectedIndex: anIndex	

]

{ #category : #api }
DropListPresenter >> setSelectedItem: anItem [
	self
		deprecated: 'Use #selectedItem: instead'
		transformWith:
			'`@receiver setSelectedItem: `@argument'
				-> '`@receiver selectedItem:`@argument'.
	^ self selectedItem: anItem	

]

{ #category : #api }
DropListPresenter >> startWithSelection [

	startsWithSelection := true
]

{ #category : #api }
DropListPresenter >> startWithoutSelection [

	startsWithSelection := false
]

{ #category : #'api-events' }
DropListPresenter >> whenSelectedItemChangedDo: aBlock [
	"Set a block to perform when the selected item is changed"

	selection whenChangedDo: [	aBlock cull: self selectedItem ]
]

{ #category : #'api-events' }
DropListPresenter >> whenSelectionChangedDo: aBlock [
	"Set a block to perform when the selection is changed"
	
	"The method should be used only if you are interested in the fact that there was a change, without caring about what has changed
	If you are interested in the items, use whenSelectedItemChanged:
	If you are interested in the index, use whenSelectionIndexChanged:"

	selection whenChangedDo: aBlock.
]

{ #category : #'api-events' }
DropListPresenter >> whenSelectionIndexChangedDo: aBlock [
	"Set a block to perform when the selected index is changed"

	self deprecated: 'Use whenSelectionChanged:'.

	selection whenSelectionChanged: aBlock
]
