"
A PickList is a tick list done using spec.

self example
"
Class {
	#name : #PickListPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'defaultValueHolder',
		'labelClickableHolder',
		'pickedItemsHolder',
		'wrapHolder',
		'associationsItemToWrapper',
		'associationsWrapperToItem',
		'associationsIndexToWrapper',
		'blockToPerformOnWrappers',
		'listPresenter'
	],
	#category : #'Spec-PolyWidgets-ListAndTree'
}

{ #category : #specs }
PickListPresenter class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed 
		add: #listPresenter;
		yourself
]

{ #category : #example }
PickListPresenter class >> example [
	| example |
	example := self new.
	example
		items:
			{{1 . 2 . 4}.
			'Foo'.
			123}.
	example openWithSpec.
			^example
]

{ #category : #specs }
PickListPresenter class >> title [

	^ 'Pick List Example'
]

{ #category : #private }
PickListPresenter >> addPicked: item [

	pickedItemsHolder add: item.
	pickedItemsHolder valueChanged: true to: item.
]

{ #category : #accessing }
PickListPresenter >> blockToPerformOnWrappers [

	^ blockToPerformOnWrappers value
]

{ #category : #accessing }
PickListPresenter >> blockToPerformOnWrappers: aBlock [

	blockToPerformOnWrappers value: aBlock
]

{ #category : #api }
PickListPresenter >> checkboxFor: anItem [

	^ associationsItemToWrapper at: anItem ifAbsent: [ nil ]
]

{ #category : #api }
PickListPresenter >> defaultValue [

	^ defaultValueHolder value
]

{ #category : #api }
PickListPresenter >> defaultValue: aBoolean [

	defaultValueHolder value: aBoolean
]

{ #category : #api }
PickListPresenter >> displayBlock: aBlock [

	wrapHolder value: aBlock
]

{ #category : #api }
PickListPresenter >> indexFor: aCheckbox [

	^ listPresenter listItems indexOf: aCheckbox.
]

{ #category : #initialization }
PickListPresenter >> initialize [

	wrapHolder := [ :i | i printString ] asValueHolder. 
	pickedItemsHolder := OrderedCollection new asValueHolder.
	associationsWrapperToItem := Dictionary new asValueHolder.
	associationsItemToWrapper := Dictionary new asValueHolder.
	associationsIndexToWrapper := Dictionary new asValueHolder.
	labelClickableHolder := true asValueHolder.
	defaultValueHolder := false asValueHolder.
	blockToPerformOnWrappers := [:wrapper | ] asValueHolder.
	
	super initialize.
]

{ #category : #initialization }
PickListPresenter >> initializePresenter [
	labelClickableHolder
		whenChangedDo:
			[ :aBoolean | associationsWrapperToItem keysDo: [ :e | e labelClickable: aBoolean ] ].
	defaultValueHolder
		whenChangedDo: [ :value | associationsWrapperToItem keysDo: [ :cb | cb state: value ] ]
]

{ #category : #initialization }
PickListPresenter >> initializeWidgets [
	
	listPresenter := self newList.
	listPresenter displayBlock: [ :item | item ].
	self focusOrder add: listPresenter
]

{ #category : #api }
PickListPresenter >> itemFor: aCheckbox [

	^ associationsWrapperToItem at: aCheckbox ifAbsent: [ nil ]
]

{ #category : #api }
PickListPresenter >> items: aCollection [

	associationsWrapperToItem removeAll.
	pickedItemsHolder removeAll.
	listPresenter items: (aCollection collectWithIndex: [:e :i | self wrap: e at: i ]).
]

{ #category : #api }
PickListPresenter >> labelClickable [
	
	^ labelClickableHolder value
]

{ #category : #api }
PickListPresenter >> labelClickable: aBoolean [

	labelClickableHolder value: aBoolean
]

{ #category : #accessing }
PickListPresenter >> listPresenter [

	^ listPresenter
]

{ #category : #api }
PickListPresenter >> pickedItems [
	"Returns the selected items according to the order they have been picked"
	
	^ pickedItemsHolder value
]

{ #category : #private }
PickListPresenter >> removePicked: item [

	pickedItemsHolder remove: item ifAbsent: [ ^ self ].
]

{ #category : #api }
PickListPresenter >> resetSelection [

	listPresenter resetSelection
]

{ #category : #api }
PickListPresenter >> selectedIndex [

	^ listPresenter selectedIndex
]

{ #category : #api }
PickListPresenter >> selectedItem [

	^ associationsWrapperToItem 
		at: listPresenter selectedItem
		ifAbsent: [ nil ]
]

{ #category : #api }
PickListPresenter >> selectedItems [
	"Returns the selected items according to the list order"
	| wrappers items |
	
	wrappers := listPresenter listItems.
	items := wrappers collect: [:e || value |
		value := associationsWrapperToItem at: e ifAbsent: [ nil ].
		(value isNil or: [ e state not ])
			ifTrue: [ nil ]
			ifFalse: [ value ]] thenSelect: [ :e | e notNil ].
	^ items
]

{ #category : #api }
PickListPresenter >> setSelectedIndex: anIndex [

	listPresenter setSelectedIndex: anIndex
]

{ #category : #'api-events' }
PickListPresenter >> whenListChanged: aBlock [

	listPresenter whenListChanged: aBlock
]

{ #category : #'api-events' }
PickListPresenter >> whenPickedItemsChanged: aBlock [

	pickedItemsHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
PickListPresenter >> whenSelectedItemChanged: aBlock [
	| newBlock |
	
	newBlock := [ :item | aBlock cull: (associationsWrapperToItem at: item ifAbsent: [ nil ])].
	listPresenter whenSelectedItemChanged: newBlock
]

{ #category : #'api-events' }
PickListPresenter >> whenSelectionChanged: aBlock [

	"The method should be used only if you are interested in the fact that there was a change, without caring about what has changed
	If you are interested in the items, use whenSelectedItemChanged:
	If you are interested in the index, use whenSelectionIndexChanged:"

	listPresenter whenSelectionChanged: aBlock.
]

{ #category : #'api-events' }
PickListPresenter >> whenSelectionIndexChanged: aBlock [

	listPresenter whenSelectionIndexChanged: aBlock
]

{ #category : #private }
PickListPresenter >> wrap: item at: index [
	| checkBox |

	associationsIndexToWrapper 
		at: index
		ifPresent: [:cb | ^ cb ].

	checkBox := self instantiate: CheckBoxPresenter.
	blockToPerformOnWrappers value value: checkBox.
	checkBox label: (wrapHolder value cull: item cull: self).
	
	checkBox whenActivatedDo: [ 
		self addPicked: item.
		listPresenter setSelectedItem: checkBox.
		listPresenter takeKeyboardFocus ].
	
	checkBox whenDeactivatedDo: [ 
		self removePicked: item.
		listPresenter setSelectedItem: checkBox.
		listPresenter takeKeyboardFocus ].
	
	checkBox 
		state: self defaultValue;
		labelClickable: self labelClickable.
		
	associationsWrapperToItem at: checkBox put: item.
	associationsItemToWrapper at: item put: checkBox.
	associationsIndexToWrapper at: index put: checkBox.
	^ checkBox
]

{ #category : #accessing }
PickListPresenter >> wrapHolder [

	^ wrapHolder
]

{ #category : #api }
PickListPresenter >> wrappers [

	^ associationsWrapperToItem keys
]
