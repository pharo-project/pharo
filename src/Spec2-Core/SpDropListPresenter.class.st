"
A DropListPresenter is a Spec presenter for drop lists.

I am assume there is a little problem on an empty list, but frankly, who creates an empty drop list ?

See DropListExample for an example
"
Class {
	#name : #SpDropListPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#model => SpObservableSlot',
		'#startsWithSelection',
		'#selection',
		'#displayBlock => SpObservableSlot',
		'#iconBlock => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpDropListPresenter class >> adapterName [

	^ #DropListAdapter
]

{ #category : #specs }
SpDropListPresenter class >> title [

	^ 'Drop List'
]

{ #category : #api }
SpDropListPresenter >> addItemLabeled: aString do: aBlock [

	self addItemLabeled: aString do: aBlock icon: nil.
]

{ #category : #api }
SpDropListPresenter >> addItemLabeled: aString do: aBlock icon: anIcon [

	| item |
	item := SpDropListItem on: aString do: aBlock.
	item icon: anIcon.

	model add: item.
	"Adding an item adds it to the selection if there is none".
	self selection isEmpty ifTrue: [ 
		self selection selectItem: item ]
]

{ #category : #api }
SpDropListPresenter >> display [
	
	^ displayBlock
]

{ #category : #api }
SpDropListPresenter >> display: aBlock [
	
	displayBlock := aBlock 

]

{ #category : #api }
SpDropListPresenter >> displayBlock [
	
	self
		deprecated: 'Please use #display instead'
		transformWith: '`@receiver displayBlock' 
						-> '`@receiver display'.
	
	^ displayBlock
]

{ #category : #api }
SpDropListPresenter >> displayBlock: aBlock [
	
	self
		deprecated: 'Please use #display: instead'
		transformWith: '`@receiver displayBlock: `@arg' 
						-> '`@receiver display: `@arg'.
	
	displayBlock := aBlock 

]

{ #category : #api }
SpDropListPresenter >> displayForItem: anItem [
	"The order of the arguments may look weird, but then it seems more natural while using the widget"

	^ self display cull: anItem model cull: anItem
]

{ #category : #api }
SpDropListPresenter >> dropListItems: dropListItems [
	"Populate the drop list with a list DropListItems"
	
	model collection: dropListItems
]

{ #category : #api }
SpDropListPresenter >> emptyList [
	
	model collection: OrderedCollection new.

]

{ #category : #api }
SpDropListPresenter >> getIconFor: anItem [

	^ self iconBlock cull: anItem model cull: anItem
]

{ #category : #api }
SpDropListPresenter >> getIndex [

	^ selection selectedIndex
]

{ #category : #api }
SpDropListPresenter >> getList [

	^ model items
]

{ #category : #testing }
SpDropListPresenter >> hasIcons [
	
	^ iconBlock isNotNil and: [ self model collection anySatisfy: [ :e | e icon isNotNil ] ]
]

{ #category : #api }
SpDropListPresenter >> iconBlock [ 
	
	^ iconBlock 
]

{ #category : #api }
SpDropListPresenter >> iconBlock: aBlock [	
	
	iconBlock := aBlock
]

{ #category : #initialization }
SpDropListPresenter >> initialize [
	super initialize.

	model := SpCollectionListModel on: #().
	selection := SpSingleSelectionMode on: self.
	displayBlock := [ :itemModel :item | item label ].
	iconBlock := [ :itemModel :item | item icon ].
	startsWithSelection := true
]

{ #category : #api }
SpDropListPresenter >> items: aList [
	"Populate the drop list with a list of ui specific items"

	"aList is a list of domain specific objects. If you want to specify more precisely the item actions, see #addItemLabeled:do:"

	| dropListItems |
	dropListItems := aList collect: [ :each | SpDropListItem on: each do: [ ] ].
	model collection: dropListItems.
	(dropListItems isNotEmpty
		and: [ startsWithSelection 
		and: [ self selection isEmpty ] ])
		ifTrue: [ self selectIndex: 1 ]
]

{ #category : #api }
SpDropListPresenter >> listItems [
	"Return the list used to populate the drop list"
	
	^ self getList collect: [ :e | e model ].
]

{ #category : #api }
SpDropListPresenter >> listSize [
	"Return the size of the list of choices"

	^ self listItems size
]

{ #category : #api }
SpDropListPresenter >> model [
	
	^ model
]

{ #category : #selection }
SpDropListPresenter >> resetSelection [
	
	self selection unselectAll 
]

{ #category : #api }
SpDropListPresenter >> selectIndex: anInteger [ 
	
	self selection selectIndex: anInteger.
	"I execute the block associated with the item"
	self selection selectedItem value
]

{ #category : #api }
SpDropListPresenter >> selectItem: anItem [ 
	
	| realItem |
	
	anItem ifNil: [ self selection unselectAll. ^self ].
	
	realItem := model collection detect: [ :each | each model = anItem ].
	self selection selectItem: realItem.
	"I execute the block associated with the item"
	realItem value
]

{ #category : #api }
SpDropListPresenter >> selectedIndex [
	"Useless method but it provides a better and more consistent API"

	^ self getIndex
]

{ #category : #api }
SpDropListPresenter >> selectedIndex: anIndex [

	"Set the index of the item you want to be selected"
	self
		deprecated: 'Use #selectIndex: instead'
		transformWith: '`@receiver selectedIndex: `@arg'
						-> '`@receiver selectIndex: `@arg'.
	
	self selectIndex: anIndex
]

{ #category : #api }
SpDropListPresenter >> selectedItem [
	^ self selection selectedItem ifNotNil: #model
]

{ #category : #api }
SpDropListPresenter >> selectedItem: anItem [
	"Set the item you want to be selected"

	self
		deprecated: 'Use #selectItem: instead'
		transformWith: '`@receiver selectedItem: `@arg'
						-> '`@receiver selectItem: `@arg'.

	self selectItem: anItem
]

{ #category : #api }
SpDropListPresenter >> selection [
	^ selection
]

{ #category : #api }
SpDropListPresenter >> sortingBlock [
	^ self model sortingBlock
]

{ #category : #api }
SpDropListPresenter >> sortingBlock: aBlock [
	self model sortingBlock: aBlock
]

{ #category : #api }
SpDropListPresenter >> startWithSelection [

	startsWithSelection := true
]

{ #category : #api }
SpDropListPresenter >> startWithoutSelection [

	startsWithSelection := false
]

{ #category : #'api-events' }
SpDropListPresenter >> whenSelectedItemChangedDo: aBlock [
	"Set a block to perform when the selected item is changed"

	selection whenChangedDo: [	aBlock cull: self selectedItem ]
]

{ #category : #'api-events' }
SpDropListPresenter >> whenSelectionChangedDo: aBlock [
	"Set a block to perform when the selection is changed"
	
	"The method should be used only if you are interested in the fact that there was a change, without caring about what has changed
	If you are interested in the items, use whenSelectedItemChanged:
	If you are interested in the index, use whenSelectionIndexChanged:"

	selection whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpDropListPresenter >> whenSortingBlockChangedDo: aBlock [
	self model whenSortingBlockChangedDo: aBlock
]
