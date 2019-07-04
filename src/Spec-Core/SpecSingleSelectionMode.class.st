Class {
	#name : #SpecSingleSelectionMode,
	#superclass : #SpecAbstractSelectionMode,
	#instVars : [
		'selectedIndexValueHolder'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #adding }
SpecSingleSelectionMode >> addSubscription: aSubscription [

	^ selectedIndexValueHolder announcer basicSubscribe: aSubscription
]

{ #category : #selecting }
SpecSingleSelectionMode >> basicSelectIndex: indexToSelect [

	selectedIndexValueHolder value: indexToSelect
]

{ #category : #testing }
SpecSingleSelectionMode >> includesIndex: anIndex [

	^ selectedIndexValueHolder value = anIndex
]

{ #category : #testing }
SpecSingleSelectionMode >> includesItem: anItem [

	^ self selectedItem = anItem
]

{ #category : #initialization }
SpecSingleSelectionMode >> initialize [

	super initialize.
	selectedIndexValueHolder := 0 asValueHolder
]

{ #category : #testing }
SpecSingleSelectionMode >> isEmpty [
	
	^ self selectedIndex = 0
]

{ #category : #selecting }
SpecSingleSelectionMode >> selectAll [

	"Do nothing"
]

{ #category : #selecting }
SpecSingleSelectionMode >> selectIndexes: aCollection [

	aCollection ifEmpty: [ ^ self unselectAll ].
	self selectIndex: aCollection first.
]

{ #category : #selecting }
SpecSingleSelectionMode >> selectItems: aCollection [

	aCollection ifEmpty: [ ^ self unselectAll ].
	self selectItem: aCollection first.
]

{ #category : #selecting }
SpecSingleSelectionMode >> selectedIndex [
	
	^ selectedIndexValueHolder value
]

{ #category : #selecting }
SpecSingleSelectionMode >> selectedIndexes [
	"This provides polymorphism with sibling classes, and answers an array with a single 
	 element."
		
	^ self isEmpty
		ifTrue: [ #() ]
		ifFalse: [ { self selectedIndex } ]
]

{ #category : #selecting }
SpecSingleSelectionMode >> selectedItem [

	self selectedIndex = 0 ifTrue: [ ^ nil ].
	^ self model at: self selectedIndex
]

{ #category : #selecting }
SpecSingleSelectionMode >> selectedItems [
	
	^ self selectedItem
		ifNotNil: [ :anItem | { anItem } ]
		ifNil: [ #() ]
]

{ #category : #accessing }
SpecSingleSelectionMode >> subscriptions [

	^ selectedIndexValueHolder announcer subscriptions subscriptions
]

{ #category : #selecting }
SpecSingleSelectionMode >> unselectAll [
	
	self selectIndex: 0
]

{ #category : #selecting }
SpecSingleSelectionMode >> unselectIndex: anInteger [ 

	(selectedIndexValueHolder value = anInteger)
		ifTrue: [ selectedIndexValueHolder value: 0 ]
]

{ #category : #events }
SpecSingleSelectionMode >> whenChangedDo: aBlockClosure [ 
	
	selectedIndexValueHolder whenChangedDo: [ aBlockClosure cull: self ]
]
