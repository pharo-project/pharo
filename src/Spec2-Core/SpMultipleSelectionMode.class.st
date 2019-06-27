Class {
	#name : #SpMultipleSelectionMode,
	#superclass : #SpAbstractSelectionMode,
	#instVars : [
		'selectedIndexesValueHolder'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #adding }
SpMultipleSelectionMode >> addSubscription: aSubscription [

	^ selectedIndexesValueHolder announcer basicSubscribe: aSubscription
]

{ #category : #selecting }
SpMultipleSelectionMode >> basicSelectIndex: indexToSelect [

	"Adding to the selection an out of range should not change selection"
	indexToSelect = 0 ifTrue: [ ^ self ].

	self selectIndexes: (selectedIndexesValueHolder value copyWith: indexToSelect)
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
	selectedIndexesValueHolder := OrderedCollection new asValueHolder
]

{ #category : #testing }
SpMultipleSelectionMode >> isEmpty [
	
	^ self selectedIndexes isEmpty
]

{ #category : #testing }
SpMultipleSelectionMode >> isMultipleSelection [
	
	^ true
]

{ #category : #selecting }
SpMultipleSelectionMode >> selectAll [
	
	self selectIndexes: (1 to: self model size)
]

{ #category : #selecting }
SpMultipleSelectionMode >> selectIndexes: aCollection [
	| indexes |
	indexes := (aCollection collect: [ :each | self withinRangeIndex: each ]) asSet.
	"If all elements are out of range, just ignore the event like for single selection."
	(indexes isNotEmpty and: [ indexes allSatisfy: [ :each | each = 0 ] ]) ifTrue: [ ^ self ].

	indexes := indexes reject: [ :each | each = 0 ].
	indexes = selectedIndexesValueHolder value asSet ifTrue: [ ^ self ].

	^ selectedIndexesValueHolder value: indexes asOrderedCollection
]

{ #category : #selecting }
SpMultipleSelectionMode >> selectItems: aCollection [
	self selectIndexes: (aCollection collect: [ :anItem | self indexOfItem: anItem ])
]

{ #category : #selecting }
SpMultipleSelectionMode >> selectedIndexes [

	^ selectedIndexesValueHolder value
]

{ #category : #selecting }
SpMultipleSelectionMode >> selectedItems [

	^ self selectedIndexes collect: [ :idx | self model at: idx ].
]

{ #category : #accessing }
SpMultipleSelectionMode >> subscriptions [

	^ selectedIndexesValueHolder announcer subscriptions subscriptions
]

{ #category : #selecting }
SpMultipleSelectionMode >> unselectAll [
	
	self selectIndexes: #()
]

{ #category : #selecting }
SpMultipleSelectionMode >> unselectIndex: anIndex [

	self selectIndexes: (selectedIndexesValueHolder value copyWithout: anIndex)
]

{ #category : #events }
SpMultipleSelectionMode >> whenChangedDo: aBlockClosure [ 
	
	selectedIndexesValueHolder whenChangedDo: [ aBlockClosure cull: self ]
]
