"
A DropListPresenter is a Spec presenter for drop lists.

I am assume there is a little problem on an empty list, but frankly, who creates an empty drop list ?

See DropListExample for an example
"
Class {
	#name : #DropListPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'listHolder',
		'selectionHolder',
		'displayBlock',
		'iconHolder'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
DropListPresenter class >> adapterName [

	^ #DropListAdapter
]

{ #category : #specs }
DropListPresenter class >> defaultSpec [
	<spec>
	
	^ #(DropListAdapter
		adapt: #(model))
]

{ #category : #specs }
DropListPresenter class >> title [

	^ 'Drop List'
]

{ #category : #api }
DropListPresenter >> addItemLabeled: aString do: aBlock [

	| item |
	item := DropListItem named: aString do: aBlock.
	
	"If you add directly in the contents, the update is not triggered from the value holder"
	listHolder add: item.
]

{ #category : #api }
DropListPresenter >> addItemLabeled: aString do: aBlock icon: anIcon [

	| item |
	item := DropListItem named: aString do: aBlock.
	item icon: anIcon.
	"If you add directly in the contents, the update is not triggered from the value holder"
	listHolder add: item.
]

{ #category : #api }
DropListPresenter >> displayBlock [
	
	^ displayBlock value
]

{ #category : #api }
DropListPresenter >> displayBlock: aBlock [
	
	displayBlock value: aBlock 
	"listHolder replace: [ :e | e display: aBlock; yourself  ]."
]

{ #category : #api }
DropListPresenter >> displayForItem: anItem [
	"The order of the arguments may look weird, but then it seems more natural while using the widget"
	
	^ self displayBlock cull: anItem model cull: anItem
]

{ #category : #api }
DropListPresenter >> emptyList [
	
	listHolder value: OrderedCollection new
]

{ #category : #api }
DropListPresenter >> getIconFor: anItem [

	^ self iconHolder cull: anItem
]

{ #category : #api }
DropListPresenter >> getIndex [

	^ selectionHolder index value
]

{ #category : #api }
DropListPresenter >> getList [

	^ listHolder value
]

{ #category : #api }
DropListPresenter >> iconHolder [

	^ iconHolder value
]

{ #category : #api }
DropListPresenter >> iconHolder: aBlock [
	
	iconHolder value: aBlock
]

{ #category : #initialization }
DropListPresenter >> initialize [

	super initialize.

	listHolder := OrderedCollection new asValueHolder.
	selectionHolder := SelectionValueHolder new.
	
	listHolder whenChangedDo: [ 
					self changed: #getList.
					self changed: #getIconFor:.
					self internalResetSelection.].
				
	selectionHolder whenChangedDo: [ self changed: #getIndex ].
	displayBlock := [ :model :item | item label ] asValueHolder.
	iconHolder := [ :model :item | item icon ] asValueHolder.
	
	self whenSelectedItemChanged: [:item | item value ]
]

{ #category : #private }
DropListPresenter >> internalResetSelection [

	selectionHolder reset.
	self getList ifNotEmpty: [ self silentlySetSelectedIndex: 1 ]
]

{ #category : #api }
DropListPresenter >> items: aList [
	"Populate the drop list with a list of ui specific items"
	"aList is a list of domain specific objects. If you want to specify more precisely the item actions, see #addItemLabeled:do:"
	"<api: #st getter: #listItems>"
	
	| dropListItems |
	dropListItems := aList collect: [ :e | DropListItem on: e do: [] ].
	listHolder value: dropListItems
]

{ #category : #api }
DropListPresenter >> listItems [
	"<api: #inspect>"
	"Return the list used to populate the drop list"
	
	^ listHolder value collect: [ :e | e model ].
]

{ #category : #api }
DropListPresenter >> listSize [
	"<api: #inspect>"
	"Return the size of the list of choices"

	^ self listItems size
]

{ #category : #api }
DropListPresenter >> resetSelection [
	"<api: #do>"
	"Reset the current selection state"

	selectionHolder reset
]

{ #category : #api }
DropListPresenter >> selectedIndex [
	"<api: #inspect>"
	"Useless method but it provides a better and more consistent API"

	^ self getIndex
]

{ #category : #private }
DropListPresenter >> selectedIndexHolder [

	^ selectionHolder index
]

{ #category : #api }
DropListPresenter >> selectedItem [
	"<api: #inspect>"

	^ self selectedItemHolder value model
]

{ #category : #private }
DropListPresenter >> selectedItemHolder [

	^ selectionHolder selection
]

{ #category : #api }
DropListPresenter >> setIndex: anIndex [
	| item |

	selectionHolder index value: anIndex.
	item := (listHolder value at: anIndex ifAbsent: [ nil ]).
	item value.
	selectionHolder selection value: item.
	self changed: #getIndex
]

{ #category : #api }
DropListPresenter >> setSelectedIndex: anIndex [
	"<api: #integer min:1 max:#listSize getter: #selectedIndex>"
	"Force the selection of the item at index anIndex"
	| selection |

	selection := listHolder value at: anIndex ifAbsent: [ ^ self ].
	selection value.
	selectionHolder index value: anIndex.
	selectionHolder selection value: selection
]

{ #category : #api }
DropListPresenter >> setSelectedItem: anItem [
	"Force the selection of the item anItem"
	"<api: #st getter: #selectedItem>"
	| index realItem |
	index := self listItems indexOf: anItem ifAbsent: [ ^ self ].
	realItem := listHolder at: index.
	selectionHolder index value: index.
	selectionHolder selection value: realItem.
]

{ #category : #private }
DropListPresenter >> silentlySetSelectedIndex: anIndex [
	"Force the selection of the item at index anIndex"
	| selection |

	selection := listHolder value at: anIndex ifAbsent: [ ^ self ].
	selectionHolder index value: anIndex.
	selectionHolder selection value: selection.
]

{ #category : #'api-events' }
DropListPresenter >> whenSelectedItemChanged: aBlock [
	"Set a block to perform when the selected item is changed"

	selectionHolder selection whenChangedDo: [ :new :old :announcement :ann | 
		aBlock cull: (new ifNotNil: [ :item | item model ]) cull: old cull: announcement cull: ann ]
]

{ #category : #'api-events' }
DropListPresenter >> whenSelectionChanged: aBlock [
	"Set a block to perform when the selection is changed"
	
	"The method should be used only if you are interested in the fact that there was a change, without caring about what has changed
	If you are interested in the items, use whenSelectedItemChanged:
	If you are interested in the index, use whenSelectionIndexChanged:"

	selectionHolder whenChangedDo: aBlock.
]

{ #category : #'api-events' }
DropListPresenter >> whenSelectionIndexChanged: aBlock [
	"Set a block to perform when the selected index is changed"

	selectionHolder index whenChangedDo: aBlock
]
