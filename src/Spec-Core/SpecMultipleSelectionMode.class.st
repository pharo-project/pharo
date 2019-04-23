Class {
	#name : #SpecMultipleSelectionMode,
	#superclass : #SpecAbstractSelectionMode,
	#instVars : [
		'selectedIndexesValueHolder'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #adding }
SpecMultipleSelectionMode >> addSubscription: aSubscription [

	^ selectedIndexesValueHolder announcer basicSubscribe: aSubscription
]

{ #category : #selecting }
SpecMultipleSelectionMode >> basicSelectIndex: indexToSelect [

	"Adding to the selection an out of range should not change selection"
	indexToSelect = 0 ifTrue: [ ^ self ].

	self selectIndexes: (selectedIndexesValueHolder value copyWith: indexToSelect)
]

{ #category : #testing }
SpecMultipleSelectionMode >> includesIndex: anIndex [

	^ self selectedIndexes includes: anIndex
]

{ #category : #testing }
SpecMultipleSelectionMode >> includesIndexes: aCollection [
	^ aCollection allSatisfy: [ :anIndex | self selectedIndexes includes: anIndex ]
]

{ #category : #testing }
SpecMultipleSelectionMode >> includesItem: anItem [

	^ self selectedItems includes: anItem
]

{ #category : #testing }
SpecMultipleSelectionMode >> includesItems: aCollection [
	^ aCollection allSatisfy: [ :anItem | self selectedItems includes: anItem ]
]

{ #category : #initialization }
SpecMultipleSelectionMode >> initialize [

	super initialize.
	selectedIndexesValueHolder := OrderedCollection new asValueHolder
]

{ #category : #testing }
SpecMultipleSelectionMode >> isEmpty [
	
	^ self selectedIndexes isEmpty
]

{ #category : #testing }
SpecMultipleSelectionMode >> isMultipleSelection [
	
	^ true
]

{ #category : #selecting }
SpecMultipleSelectionMode >> selectAll [
	
	self selectIndexes: (1 to: self model size)
]

{ #category : #selecting }
SpecMultipleSelectionMode >> selectIndexes: aCollection [
	| indexes |
	indexes := (aCollection collect: [ :each | self withinRangeIndex: each ]) asSet.
	"If all elements are out of range, just ignore the event like for single selection."
	(indexes isNotEmpty and: [ indexes allSatisfy: [ :each | each = 0 ] ]) ifTrue: [ ^ self ].

	indexes := indexes reject: [ :each | each = 0 ].
	indexes = selectedIndexesValueHolder value asSet ifTrue: [ ^ self ].

	^ selectedIndexesValueHolder value: indexes asOrderedCollection
]

{ #category : #selecting }
SpecMultipleSelectionMode >> selectItems: aCollection [
	self selectIndexes: (aCollection collect: [ :anItem | self indexOfItem: anItem ])
]

{ #category : #selecting }
SpecMultipleSelectionMode >> selectedIndexes [

	^ selectedIndexesValueHolder value
]

{ #category : #selecting }
SpecMultipleSelectionMode >> selectedItems [

	^ self selectedIndexes collect: [ :idx | self model at: idx ].
]

{ #category : #accessing }
SpecMultipleSelectionMode >> subscriptions [

	^ selectedIndexesValueHolder announcer subscriptions
]

{ #category : #selecting }
SpecMultipleSelectionMode >> unselectAll [
	
	self selectIndexes: #()
]

{ #category : #selecting }
SpecMultipleSelectionMode >> unselectIndex: anIndex [

	self selectIndexes: (selectedIndexesValueHolder value copyWithout: anIndex)
]

{ #category : #events }
SpecMultipleSelectionMode >> whenChangedDo: aBlockClosure [ 
	
	selectedIndexesValueHolder whenChangedDo: [ aBlockClosure cull: self ]
]
