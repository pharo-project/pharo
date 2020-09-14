"
A presenter to display dropdown lists.

_NOTE: I assume there is a little problem on an empty list, but frankly, who creates an empty drop list?_

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

{ #category : #documentation }
SpDropListPresenter class >> documentFactoryMethodSelector [

	^ #newDropList
]

{ #category : #specs }
SpDropListPresenter class >> title [

	^ 'Drop List'
]

{ #category : #api }
SpDropListPresenter >> addItemLabeled: aString do: aBlock [
	"Add an item to the drop list, along with an action.
	 `aString` is the label of the element.
	 `aBlock` is the action asociated to this element. It receives zero arguments."

	self addItemLabeled: aString do: aBlock icon: nil.
]

{ #category : #api }
SpDropListPresenter >> addItemLabeled: aString do: aBlock icon: anIcon [
	"Add an item to the drop list, along with an icon and an action.
	 `aString` is the label of the element.
	 `aBlock` is the action asociated to this element. It receives zero arguments.
	 `anIcon` is the icon (an instance of `Form`) to be shown."
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
	"Answer the formatting block to transform how the elements will be displayed. 
	 See also `SpDropListPresenter>>#display:`"
	
	^ displayBlock
]

{ #category : #api }
SpDropListPresenter >> display: aBlock [
	"Set the formatting block to transform how the elements will be displayed. 
	 `aBlock` receives one argument (the element to be displayed) and it should answer a string.
	 See also `SpDropListPresenter>>#display`"

	displayBlock := aBlock 

]

{ #category : #private }
SpDropListPresenter >> displayForItem: anItem [
	"The order of the arguments may look weird, but then it seems more natural while using the widget"

	^ self display cull: anItem model cull: anItem
]

{ #category : #private }
SpDropListPresenter >> dropListItems: dropListItems [
	"Populate the drop list with a list DropListItems"
	
	model collection: dropListItems
]

{ #category : #api }
SpDropListPresenter >> emptyList [
	"Empty the list of elements used to populate the drop down list."
	
	model collection: OrderedCollection new.

]

{ #category : #private }
SpDropListPresenter >> getIconFor: anItem [

	^ self iconBlock cull: anItem model cull: anItem
]

{ #category : #private }
SpDropListPresenter >> getIndex [

	^ selection selectedIndex
]

{ #category : #private }
SpDropListPresenter >> getList [

	^ model items
]

{ #category : #testing }
SpDropListPresenter >> hasIcons [
	"Answer whether the dropdown list will show icons"
	
	^ iconBlock isNotNil and: [ self model collection anySatisfy: [ :e | e icon isNotNil ] ]
]

{ #category : #api }
SpDropListPresenter >> iconBlock [ 
	"Answer the block that will be used to retrieve the icon to be displayed along with the 
	 elements."
	
	self flag: #TODO. "Rename as just #icon"
	^ iconBlock
]

{ #category : #api }
SpDropListPresenter >> iconBlock: aBlock [	
	"Answer a block that will be used to retrieve the icon to be displayed along with the elements.
	 `aBlock` receives one argument (an element from the list)"
	
	self flag: #TODO. "Rename as just #icon:"
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
SpDropListPresenter >> items [ 
	"Return the list used to populate the drop list"

	^ self listItems
]

{ #category : #api }
SpDropListPresenter >> items: aList [
	"Populate the drop list with a list of ui specific items"
	"`aList` is a list of domain specific objects. If you want to specify more precisely the item actions, see `SpDropListPresenter>>#addItemLabeled:do:`"
	| dropListItems |

	dropListItems := aList collect: [ :each | SpDropListItem on: each do: [ ] ].
	model collection: dropListItems.
	(dropListItems isNotEmpty
		and: [ startsWithSelection 
		and: [ self selection isEmpty ] ])
		ifTrue: [ self selectIndex: 1 ]
		ifFalse: [ self resetSelection ]
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
	"Answer the model containing the elements to be shown in the drop down list.
	 Tipically, the model is a collection of `SpDropListItem`."
	
	^ model
]

{ #category : #selection }
SpDropListPresenter >> resetSelection [
	
	self selection unselectAll 
]

{ #category : #api }
SpDropListPresenter >> selectIndex: anInteger [ 
	"Select the element at position `anInteger` and executes the action associated with it."
	
	self selection selectIndex: anInteger.
	"I execute the block associated with the item"
	self selection selectedItem value
]

{ #category : #api }
SpDropListPresenter >> selectItem: anItem [ 
	"Select the element `anItem` if it is in the list. 
	 It executes the action associated with the item if it is defined."
	| realItem |
	
	anItem ifNil: [ 
		self selection unselectAll. 
		^ self ].
	
	realItem := model collection detect: [ :each | each model = anItem ].
	self selection selectItem: realItem.
	"I execute the block associated with the item"
	realItem value
]

{ #category : #api }
SpDropListPresenter >> selectedIndex [
	"Answer the index of selected item. 
	 You usually do not need to use this method but `SpDropListPresenter>>#selectedItem`."

	^ self getIndex
]

{ #category : #api }
SpDropListPresenter >> selectedItem [
	"Answer selected item"

	^ self selection selectedItem 
		ifNotNil: [ :anItem | anItem model ]
]

{ #category : #api }
SpDropListPresenter >> selection [
	"Answer selection model, an instance of `SpSingleSelectionMode`."
	
	^ selection
]

{ #category : #api }
SpDropListPresenter >> sortingBlock [
	"Answer the block used to sort the elements of the list."

	^ self model sortingBlock
]

{ #category : #api }
SpDropListPresenter >> sortingBlock: aBlock [
	"Set the block or `SortFunction` used to sort the elements of the list."

	self model sortingBlock: aBlock
]

{ #category : #api }
SpDropListPresenter >> startWithSelection [
	"Indicate when drop list will start with an element selected."

	startsWithSelection := true
]

{ #category : #api }
SpDropListPresenter >> startWithoutSelection [
	"Indicate when droplist will start without an element selected."

	startsWithSelection := false
]

{ #category : #'api-events' }
SpDropListPresenter >> whenSelectedItemChangedDo: aBlock [
	"Inform when the selected item is changed.
	 `aBlock` receive 3 optional arguments: 
	 - new value 
	 - old value 
	 - announcement triggered"

	selection whenChangedDo: [	aBlock cull: self selectedItem ]
]

{ #category : #'api-events' }
SpDropListPresenter >> whenSelectionChangedDo: aBlock [
	"Inform when the selection is changed.
	 The method should be used only if you are interested in the fact that there was 
	 a change, without caring about what has changed.
	 If you are interested in the items, use `SpDropListPresenter>>#whenSelectedItemChanged:`
	
	 `aBlock` receive 3 optional arguments: 
	 - new value 
	 - old value 
	 - announcement triggered"


	selection whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpDropListPresenter >> whenSortingBlockChangedDo: aBlock [
	"Inform when sorting block has changed.
	 `aBlock` receive 3 optional arguments: 
	 - new value 
	 - old value 
	 - announcement triggered"

	self model whenSortingBlockChangedDo: aBlock
]
