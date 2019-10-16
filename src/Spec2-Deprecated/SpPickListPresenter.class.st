"
A PickList is a tick list done using spec.

self example
"
Class {
	#name : #SpPickListPresenter,
	#superclass : #SpPresenter,
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
	#category : #'Spec2-Deprecated-Obsolete'
}

{ #category : #specs }
SpPickListPresenter class >> defaultSpec [
	<spec>
	
	^ SpLayout composed 
		add: #listPresenter;
		yourself
]

{ #category : #deprecation }
SpPickListPresenter class >> example [
	<sampleInstance>
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

{ #category : #deprecation }
SpPickListPresenter class >> isDeprecated [
	"This presenter does not add enough value to Spec to be kept in it. If you are using it, please copy the code in your own application."

	^ true
]

{ #category : #specs }
SpPickListPresenter class >> title [

	^ 'Pick List Example'
]

{ #category : #private }
SpPickListPresenter >> addPicked: item [

	pickedItemsHolder add: item.
	pickedItemsHolder valueChanged: true to: item.
]

{ #category : #accessing }
SpPickListPresenter >> blockToPerformOnWrappers [

	^ blockToPerformOnWrappers value
]

{ #category : #accessing }
SpPickListPresenter >> blockToPerformOnWrappers: aBlock [

	blockToPerformOnWrappers value: aBlock
]

{ #category : #api }
SpPickListPresenter >> checkboxFor: anItem [

	^ associationsItemToWrapper at: anItem ifAbsent: [ nil ]
]

{ #category : #initialization }
SpPickListPresenter >> connectPresenters [
	labelClickableHolder
		whenChangedDo:
			[ :aBoolean | associationsWrapperToItem keysDo: [ :e | e labelClickable: aBoolean ] ].
	defaultValueHolder
		whenChangedDo: [ :value | associationsWrapperToItem keysDo: [ :cb | cb state: value ] ]
]

{ #category : #api }
SpPickListPresenter >> defaultValue [

	^ defaultValueHolder value
]

{ #category : #api }
SpPickListPresenter >> defaultValue: aBoolean [

	defaultValueHolder value: aBoolean
]

{ #category : #api }
SpPickListPresenter >> displayBlock: aBlock [

	wrapHolder value: aBlock
]

{ #category : #api }
SpPickListPresenter >> indexFor: aCheckbox [

	^ listPresenter listItems indexOf: aCheckbox.
]

{ #category : #initialization }
SpPickListPresenter >> initialize [

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
SpPickListPresenter >> initializePresenters [
	listPresenter := self newList.
	listPresenter display: [ :item | item ].
	self focusOrder add: listPresenter
]

{ #category : #api }
SpPickListPresenter >> itemFor: aCheckbox [

	^ associationsWrapperToItem at: aCheckbox ifAbsent: [ nil ]
]

{ #category : #api }
SpPickListPresenter >> items: aCollection [

	associationsWrapperToItem removeAll.
	pickedItemsHolder removeAll.
	listPresenter items: (aCollection collectWithIndex: [:e :i | self wrap: e at: i ]).
]

{ #category : #api }
SpPickListPresenter >> labelClickable [
	
	^ labelClickableHolder value
]

{ #category : #api }
SpPickListPresenter >> labelClickable: aBoolean [

	labelClickableHolder value: aBoolean
]

{ #category : #accessing }
SpPickListPresenter >> listPresenter [

	^ listPresenter
]

{ #category : #api }
SpPickListPresenter >> pickedItems [
	"Returns the selected items according to the order they have been picked"
	
	^ pickedItemsHolder value
]

{ #category : #private }
SpPickListPresenter >> removePicked: item [

	pickedItemsHolder remove: item ifAbsent: [ ^ self ].
]

{ #category : #api }
SpPickListPresenter >> resetSelection [

	listPresenter resetSelection
]

{ #category : #api }
SpPickListPresenter >> selectedIndex [

	^ listPresenter selectedIndex
]

{ #category : #api }
SpPickListPresenter >> selectedItem [

	^ associationsWrapperToItem 
		at: listPresenter selectedItem
		ifAbsent: [ nil ]
]

{ #category : #api }
SpPickListPresenter >> selectedItems [
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
SpPickListPresenter >> setSelectedIndex: anIndex [

	listPresenter setSelectedIndex: anIndex
]

{ #category : #'api-events' }
SpPickListPresenter >> whenListChanged: aBlock [

	listPresenter whenListChanged: aBlock
]

{ #category : #'api-events' }
SpPickListPresenter >> whenPickedItemsChanged: aBlock [

	pickedItemsHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpPickListPresenter >> whenSelectedItemChanged: aBlock [
	| newBlock |
	
	newBlock := [ :item | aBlock cull: (associationsWrapperToItem at: item ifAbsent: [ nil ])].
	listPresenter whenSelectedItemChanged: newBlock
]

{ #category : #'api-events' }
SpPickListPresenter >> whenSelectionChanged: aBlock [

	"The method should be used only if you are interested in the fact that there was a change, without caring about what has changed
	If you are interested in the items, use whenSelectedItemChanged:
	If you are interested in the index, use whenSelectionIndexChanged:"

	listPresenter whenSelectionChanged: aBlock.
]

{ #category : #'api-events' }
SpPickListPresenter >> whenSelectionIndexChanged: aBlock [

	listPresenter whenSelectionIndexChanged: aBlock
]

{ #category : #private }
SpPickListPresenter >> wrap: item at: index [
	| checkBox |
	associationsIndexToWrapper at: index ifPresent: [ :cb | ^ cb ].
	checkBox := self instantiate: SpCheckBoxPresenter.
	blockToPerformOnWrappers value value: checkBox.
	checkBox label: (wrapHolder value cull: item cull: self).
	checkBox
		whenActivatedDo: [ self addPicked: item.
			listPresenter setSelectedItem: checkBox.
			listPresenter takeKeyboardFocus ].
	checkBox
		whenDeactivatedDo: [ self removePicked: item.
			listPresenter selectItem: checkBox.
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
SpPickListPresenter >> wrapHolder [

	^ wrapHolder
]

{ #category : #api }
SpPickListPresenter >> wrappers [

	^ associationsWrapperToItem keys
]
