"
I am a base selection mode. 
My children will define selection styles (single, multiple) and realise the different selecting logic. 

I can : 

- selectItem/selectItems
- selectIndex/selectIndexes
- selectAll

Also, I can retrieve 

- selectedItems
- selectedIndexes

"
Class {
	#name : #SpAbstractSelectionMode,
	#superclass : #Object,
	#traits : 'SpTObservable',
	#classTraits : 'SpTObservable classTrait',
	#instVars : [
		'widget'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #'instance creation' }
SpAbstractSelectionMode class >> on: aWidget [

	^ self new
		widget: aWidget;
		yourself
]

{ #category : #selecting }
SpAbstractSelectionMode >> basicSelectIndex: indexToSelect [

	self subclassResponsibility
]

{ #category : #testing }
SpAbstractSelectionMode >> includesIndex: anIndex [

	self subclassResponsibility
]

{ #category : #testing }
SpAbstractSelectionMode >> includesItem: anItem [

	self subclassResponsibility
]

{ #category : #selecting }
SpAbstractSelectionMode >> indexOfItem: anItem [
	
	^ self model
		indexOf: anItem
		ifAbsent: [ 0 ].
]

{ #category : #initialization }
SpAbstractSelectionMode >> initialize [
	self class initializeSlots: self.
	super initialize
]

{ #category : #testing }
SpAbstractSelectionMode >> isEmpty [
	
	^ self subclassResponsibility
]

{ #category : #testing }
SpAbstractSelectionMode >> isMultipleSelection [
	
	^ false
]

{ #category : #accessing }
SpAbstractSelectionMode >> model [
	^ widget model
]

{ #category : #selecting }
SpAbstractSelectionMode >> selectAll [

	^ self subclassResponsibility
]

{ #category : #selecting }
SpAbstractSelectionMode >> selectIndex: anIndex [
	
	self basicSelectIndex: (self withinRangeIndex: anIndex)
]

{ #category : #selecting }
SpAbstractSelectionMode >> selectIndexes: aCollection [
	self subclassResponsibility
]

{ #category : #selecting }
SpAbstractSelectionMode >> selectItem: anItem [
	
	self basicSelectIndex: (self indexOfItem: anItem)
]

{ #category : #selecting }
SpAbstractSelectionMode >> selectItems: aCollection [
	self subclassResponsibility
]

{ #category : #selecting }
SpAbstractSelectionMode >> selectedIndexes [

	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractSelectionMode >> selectionHolder [
	^ self subclassResponsibility
]

{ #category : #private }
SpAbstractSelectionMode >> transferSubscriptionsTo: anotherSelectionMode [
	self selectionHolder transferSubscriptionsTo: anotherSelectionMode selectionHolder
]

{ #category : #selecting }
SpAbstractSelectionMode >> unselectAll [
	
	self subclassResponsibility
]

{ #category : #selecting }
SpAbstractSelectionMode >> unselectIndex: anInteger [ 
	
	self subclassResponsibility
]

{ #category : #selecting }
SpAbstractSelectionMode >> unselectItem: anInteger [ 
	
	self unselectIndex: (self indexOfItem: anInteger)
]

{ #category : #events }
SpAbstractSelectionMode >> whenChangedDo: aBlock [ 
	
	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractSelectionMode >> widget [
	^ widget
]

{ #category : #accessing }
SpAbstractSelectionMode >> widget: anObject [
	widget := anObject
]

{ #category : #selecting }
SpAbstractSelectionMode >> withinRangeIndex: anIndex [
	
	| indexToSelect |
	indexToSelect := anIndex.
	self model
		at: indexToSelect
		ifAbsent: [ indexToSelect := 0 ].
	^ indexToSelect
]
