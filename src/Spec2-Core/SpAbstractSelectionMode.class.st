Class {
	#name : #SpAbstractSelectionMode,
	#superclass : #Object,
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

{ #category : #adding }
SpAbstractSelectionMode >> addSubscription: aSubscription [

	^ self subclassResponsibility
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
SpAbstractSelectionMode >> subscriptions [

	^ self subclassResponsibility
]

{ #category : #private }
SpAbstractSelectionMode >> transferSubscriptionsTo: anotherSelectionMode [

	self subscriptions do: [ :aSubscription | anotherSelectionMode addSubscription: aSubscription ].
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
SpAbstractSelectionMode >> whenChangedDo: aBlockClosure [ 
	
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
