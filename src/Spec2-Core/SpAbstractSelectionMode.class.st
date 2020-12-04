"
A base for selection modes. 
`SpAbstractSelectionMode` children will define selection styles (single, multiple) and realise the different selecting logic. 

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

{ #category : #documentation }
SpAbstractSelectionMode class >> addDocumentSectionHierarchy: aBuilder [
	
	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Hierarchy' ] withLevel: 2.
	SpDocumentHierarchyBuilder new
		fromClass: SpAbstractSelectionMode;
		builder: aBuilder;
		filter: [ :eachClass | eachClass package packageName beginsWith: 'Spec2-' ];
		buildFor: self
]

{ #category : #documentation }
SpAbstractSelectionMode class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	super buildMicroDownUsing: aBuilder withComment: aString.
	"Sections"
	self documentSections keysAndValuesDo: [ :label :methods |
		SpAbstractWidgetPresenter 
			addDocumentSection: aBuilder 
			label: label 
			methods: methods ].
	"Hierarchy"
	self addDocumentSectionHierarchy: aBuilder.

]

{ #category : #documentation }
SpAbstractSelectionMode class >> documentSections [
		
	^ OrderedDictionary newFromPairs: {
		'API Methods'. self methods select: [ :each | each protocol = #'api' ].
		'API Selection Methods'. self methods select: [ :each | each protocol = #'api-selection' ].
		'Testing Methods'. self methods select: [ :each | each protocol = #'testing' ].
		'Events'. self methods select: [ :each | each protocol = #'api-events' ].
	}
]

{ #category : #'instance creation' }
SpAbstractSelectionMode class >> on: aWidget [

	^ self new
		widget: aWidget;
		yourself
]

{ #category : #private }
SpAbstractSelectionMode >> basicSelectIndex: indexToSelect [

	self subclassResponsibility
]

{ #category : #testing }
SpAbstractSelectionMode >> includesIndex: anIndex [
	"Answer true if selection includes element at index `anIndex`."

	self subclassResponsibility
]

{ #category : #testing }
SpAbstractSelectionMode >> includesItem: anItem [
	"Answer true if selection includes element `anItem`."

	self subclassResponsibility
]

{ #category : #private }
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
	"Answer true if there are no selections"
	
	^ self subclassResponsibility
]

{ #category : #testing }
SpAbstractSelectionMode >> isMultipleSelection [
	"Answer true if this is a multiple selection mode."
	
	^ false
]

{ #category : #api }
SpAbstractSelectionMode >> model [
	"Answer the model used to act the selection.
	 See also `SpAbstractListPresenter>>#model`."

	^ widget model
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> selectAll [
	"Select all items."

	^ self subclassResponsibility
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> selectIndex: anIndex [
	"Select item at position `anInteger`."
	
	self basicSelectIndex: (self withinRangeIndex: anIndex)
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> selectIndexes: aCollectionOfIndexes [
	"Select items at positions included in `aCollectionOfIndexes`."

	self subclassResponsibility
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> selectItem: anItem [
	"Select `anItem` if it is included in model list."
	
	self basicSelectIndex: (self indexOfItem: anItem)
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> selectItems: aCollection [
	"Select items included in `aCollection` if they are included in model list."
	
	self subclassResponsibility
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> selectedIndexes [
	"Answer a collection with indexes of all selected items."

	self subclassResponsibility
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> selectedItem [
	"Answer selected item"

	^ self subclassResponsibility
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> selectedItems [
	"Answer a collection with all selected items."

	^ self subclassResponsibility
]

{ #category : #private }
SpAbstractSelectionMode >> selectionHolder [
	^ self subclassResponsibility
]

{ #category : #private }
SpAbstractSelectionMode >> transferSubscriptionsTo: anotherSelectionMode [
	self selectionHolder transferSubscriptionsTo: anotherSelectionMode selectionHolder
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> unselectAll [
	"Remove all selections."
	
	self subclassResponsibility
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> unselectIndex: anInteger [ 
	"Unselect item at position `anInteger`."
	
	self subclassResponsibility
]

{ #category : #'api-selection' }
SpAbstractSelectionMode >> unselectItem: anObject [
	"Unselect item `anObject`."
	
	self unselectIndex: (self indexOfItem: anObject)
]

{ #category : #'api-events' }
SpAbstractSelectionMode >> whenChangedDo: aBlock [ 
	"Inform when selection has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"
	
	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractSelectionMode >> widget [
	"Returns the presenter"
	^ widget
]

{ #category : #accessing }
SpAbstractSelectionMode >> widget: anObject [
	widget := anObject
]

{ #category : #private }
SpAbstractSelectionMode >> withinRangeIndex: anIndex [
	
	| indexToSelect |
	indexToSelect := anIndex.
	self model
		at: indexToSelect
		ifAbsent: [ indexToSelect := 0 ].
	^ indexToSelect
]
