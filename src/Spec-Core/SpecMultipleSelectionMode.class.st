Class {
	#name : #SpecMultipleSelectionMode,
	#superclass : #SpecAbstractSelectionMode,
	#instVars : [
		'selectedIndexesValueHolder'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #'as yet unclassified' }
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
SpecMultipleSelectionMode >> includesItem: anItem [

	^ self selectedItems includes: anItem
]

{ #category : #initialization }
SpecMultipleSelectionMode >> initialize [

	super initialize.
	selectedIndexesValueHolder := OrderedCollection new asValueHolder
]

{ #category : #selecting }
SpecMultipleSelectionMode >> isEmpty [ 
	^ selectedIndexesValueHolder value isEmpty
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

	aCollection asSet = selectedIndexesValueHolder value asSet
		ifTrue: [ ^ self ].

	^ selectedIndexesValueHolder value: aCollection asSet asOrderedCollection
]

{ #category : #selecting }
SpecMultipleSelectionMode >> selectedIndexes [

	^ selectedIndexesValueHolder value
]

{ #category : #selecting }
SpecMultipleSelectionMode >> selectedItems [

	^ self selectedIndexes collect: [ :idx | self model at: idx ].
]

{ #category : #'as yet unclassified' }
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
