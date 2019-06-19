Class {
	#name : #SpecAbstractSelectionMode,
	#superclass : #Object,
	#instVars : [
		'widget'
	],
	#category : #'Spec-Core-Widgets-Table'
}

{ #category : #'instance creation' }
SpecAbstractSelectionMode class >> on: aWidget [

	^ self new
		widget: aWidget;
		yourself
]

{ #category : #adding }
SpecAbstractSelectionMode >> addSubscription: aSubscription [

	^ self subclassResponsibility
]

{ #category : #selecting }
SpecAbstractSelectionMode >> basicSelectIndex: indexToSelect [

	self subclassResponsibility
]

{ #category : #testing }
SpecAbstractSelectionMode >> includesIndex: anIndex [

	self subclassResponsibility
]

{ #category : #testing }
SpecAbstractSelectionMode >> includesItem: anItem [

	self subclassResponsibility
]

{ #category : #selecting }
SpecAbstractSelectionMode >> indexOfItem: anItem [
	
	^ self model
		indexOf: anItem
		ifAbsent: [ 0 ].
]

{ #category : #testing }
SpecAbstractSelectionMode >> isEmpty [
	
	^ self subclassResponsibility
]

{ #category : #testing }
SpecAbstractSelectionMode >> isMultipleSelection [
	
	^ false
]

{ #category : #accessing }
SpecAbstractSelectionMode >> model [
	^ widget model
]

{ #category : #selecting }
SpecAbstractSelectionMode >> selectAll [

	^ self subclassResponsibility
]

{ #category : #selecting }
SpecAbstractSelectionMode >> selectIndex: anIndex [
	
	self basicSelectIndex: (self withinRangeIndex: anIndex)
]

{ #category : #selecting }
SpecAbstractSelectionMode >> selectIndexes: aCollection [
	self subclassResponsibility
]

{ #category : #selecting }
SpecAbstractSelectionMode >> selectItem: anItem [
	
	self basicSelectIndex: (self indexOfItem: anItem)
]

{ #category : #selecting }
SpecAbstractSelectionMode >> selectItems: aCollection [
	self subclassResponsibility
]

{ #category : #selecting }
SpecAbstractSelectionMode >> selectedIndexes [

	self subclassResponsibility
]

{ #category : #accessing }
SpecAbstractSelectionMode >> subscriptions [

	^ self subclassResponsibility
]

{ #category : #private }
SpecAbstractSelectionMode >> transferSubscriptionsTo: anotherSelectionMode [

	self subscriptions do: [ :aSubscription | anotherSelectionMode addSubscription: aSubscription ].
]

{ #category : #selecting }
SpecAbstractSelectionMode >> unselectAll [
	
	self subclassResponsibility
]

{ #category : #selecting }
SpecAbstractSelectionMode >> unselectIndex: anInteger [ 
	
	self subclassResponsibility
]

{ #category : #selecting }
SpecAbstractSelectionMode >> unselectItem: anInteger [ 
	
	self unselectIndex: (self indexOfItem: anInteger)
]

{ #category : #events }
SpecAbstractSelectionMode >> whenChangedDo: aBlockClosure [ 
	
	self subclassResponsibility
]

{ #category : #accessing }
SpecAbstractSelectionMode >> widget [
	^ widget
]

{ #category : #accessing }
SpecAbstractSelectionMode >> widget: anObject [
	widget := anObject
]

{ #category : #selecting }
SpecAbstractSelectionMode >> withinRangeIndex: anIndex [
	
	| indexToSelect |
	indexToSelect := anIndex.
	self model
		at: indexToSelect
		ifAbsent: [ indexToSelect := 0 ].
	^ indexToSelect
]
