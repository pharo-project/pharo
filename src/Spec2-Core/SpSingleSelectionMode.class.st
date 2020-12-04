"
I implement single selection mode (my users can select just one element of list or table)
"
Class {
	#name : #SpSingleSelectionMode,
	#superclass : #SpAbstractSelectionMode,
	#instVars : [
		'#selectedIndex => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #private }
SpSingleSelectionMode >> basicSelectIndex: indexToSelect [
	selectedIndex := indexToSelect
]

{ #category : #testing }
SpSingleSelectionMode >> includesIndex: anIndex [
	^ self selectedIndex = anIndex
]

{ #category : #testing }
SpSingleSelectionMode >> includesItem: anItem [

	^ self selectedItem = anItem
]

{ #category : #initialization }
SpSingleSelectionMode >> initialize [
	super initialize.
	selectedIndex := 0
]

{ #category : #testing }
SpSingleSelectionMode >> isEmpty [
	
	^ self selectedIndex = 0
]

{ #category : #'api-selection' }
SpSingleSelectionMode >> selectAll [
	"Do nothing, since single selection mode wont allow for selecting all elements."
]

{ #category : #'api-selection' }
SpSingleSelectionMode >> selectIndexes: aCollectionOfIndexes [
	"Take first index from `aCollectionOfIndexes` and selects it. Ignore all the rest."

	aCollectionOfIndexes ifEmpty: [ ^ self unselectAll ].
	self selectIndex: aCollectionOfIndexes first.
]

{ #category : #'api-selection' }
SpSingleSelectionMode >> selectItems: aCollection [
	"Take first element from `aCollection` and selects it. Ignore all the rest."

	aCollection ifEmpty: [ ^ self unselectAll ].
	self selectItem: aCollection first.
]

{ #category : #'api-selection' }
SpSingleSelectionMode >> selectedIndex [
	"Answer index of current selected element"

	^ selectedIndex
]

{ #category : #'api-selection' }
SpSingleSelectionMode >> selectedIndexes [
	"Answer an array with a single element containing current selected element index."
		
	^ self isEmpty
		ifTrue: [ #() ]
		ifFalse: [ { self selectedIndex } ]
]

{ #category : #'api-selection' }
SpSingleSelectionMode >> selectedItem [
	"Answer selected item"

	self selectedIndex = 0 ifTrue: [ ^ nil ].
	self widget withAdapterDo: [ :anAdapter |
		^ anAdapter elementAt: self selectedIndex ].
	^ self model at: self selectedIndex
]

{ #category : #'api-selection' }
SpSingleSelectionMode >> selectedItems [
	"Answer an array with a single element, the selected item"
	
	^ self selectedItem
		ifNotNil: [ :anItem | { anItem } ]
		ifNil: [ #() ]
]

{ #category : #private }
SpSingleSelectionMode >> selectionHolder [
	^ self observablePropertyNamed: #selectedIndex
]

{ #category : #'api-selection' }
SpSingleSelectionMode >> unselectAll [
	"Unselect current selected element."
	
	self selectIndex: 0
]

{ #category : #'api-selection' }
SpSingleSelectionMode >> unselectIndex: anInteger [
	"Unselect element at index `anInteger`"

	self selectedIndex = anInteger ifTrue: [ self basicSelectIndex: 0 ]
]

{ #category : #'api-events' }
SpSingleSelectionMode >> whenChangedDo: aBlock [
	self property: #selectedIndex whenChangedDo: [ aBlock cull: self ]
]
