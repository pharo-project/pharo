"
Implement multiple selection mode (Users can select multiple element of list or table)
"
Class {
	#name : #SpMultipleSelectionMode,
	#superclass : #SpAbstractSelectionMode,
	#instVars : [
		'#selectedIndexes => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #private }
SpMultipleSelectionMode >> basicSelectIndex: indexToSelect [

	"Adding to the selection an out of range should not change selection"
	indexToSelect = 0 ifTrue: [ ^ self ].

	self selectIndexes: (self selectedIndexes copyWith: indexToSelect)
]

{ #category : #testing }
SpMultipleSelectionMode >> includesIndex: anIndex [

	^ self selectedIndexes includes: anIndex
]

{ #category : #testing }
SpMultipleSelectionMode >> includesIndexes: aCollection [
	^ aCollection allSatisfy: [ :anIndex | self selectedIndexes includes: anIndex ]
]

{ #category : #testing }
SpMultipleSelectionMode >> includesItem: anItem [

	^ self selectedItems includes: anItem
]

{ #category : #testing }
SpMultipleSelectionMode >> includesItems: aCollection [
	^ aCollection allSatisfy: [ :anItem | self selectedItems includes: anItem ]
]

{ #category : #initialization }
SpMultipleSelectionMode >> initialize [
	super initialize.
	selectedIndexes := OrderedCollection new
]

{ #category : #testing }
SpMultipleSelectionMode >> isEmpty [
	
	^ self selectedIndexes isEmpty
]

{ #category : #testing }
SpMultipleSelectionMode >> isMultipleSelection [
	
	^ true
]

{ #category : #'api-selection' }
SpMultipleSelectionMode >> selectAll [
	"Select all items."
	
	self selectIndexes: (1 to: self model size)
]

{ #category : #'api-selection' }
SpMultipleSelectionMode >> selectIndexes: aCollectionOfIndexes [
	"Select items at positions included in `aCollectionOfIndexes`."
	| indexes |

	indexes := aCollectionOfIndexes 
		collect: [ :each | self withinRangeIndex: each ]
		thenReject: [ :each | each = 0 ]. "0 index means out of range"
	(aCollectionOfIndexes isNotEmpty and: [ indexes isEmpty ]) "all indexes are out of range"
		ifTrue: [ ^ #() ].

	indexes := indexes asOrderedCollection removeDuplicates.
	indexes = self selectedIndexes ifTrue: [ ^ self ].

	^ selectedIndexes := indexes
]

{ #category : #'api-selection' }
SpMultipleSelectionMode >> selectItems: aCollection [
	"Select items included in `aCollection` if they are included in model list."

	self selectIndexes: (aCollection collect: [ :anItem | self indexOfItem: anItem ])
]

{ #category : #'api-selection' }
SpMultipleSelectionMode >> selectedIndexes [
	"Answer a collection with indexes of all selected items."

	^ selectedIndexes
]

{ #category : #'api-selection' }
SpMultipleSelectionMode >> selectedItem [
	"Answer first selected item."

	^ self selectedItems 
		ifNotEmpty: [ :items | items first ]
		ifEmpty: [ nil ]
]

{ #category : #'api-selection' }
SpMultipleSelectionMode >> selectedItems [
	"Answer a collection with all selected items."

	^ self widget itemsAt: self selectedIndexes
]

{ #category : #private }
SpMultipleSelectionMode >> selectedItemsSortedByIndex [

	^ self widget itemsAt: self selectedIndexes sort
]

{ #category : #private }
SpMultipleSelectionMode >> selectionHolder [
	^ self observablePropertyNamed: #selectedIndexes
]

{ #category : #'api-selection' }
SpMultipleSelectionMode >> unselectAll [
	"Remove all selections."
	
	self selectIndexes: #()
]

{ #category : #'api-selection' }
SpMultipleSelectionMode >> unselectIndex: anIndex [
	"Unselect item at position `anInteger`."

	self selectIndexes: (self selectedIndexes copyWithout: anIndex)
]

{ #category : #'api-events' }
SpMultipleSelectionMode >> whenChangedDo: aBlock [
	"Inform when selection has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #selectedIndexes whenChangedDo: [ aBlock cull: self ]
]
