Class {
	#name : #SpAbstractTreeSingleSelectionMode,
	#superclass : #Object,
	#traits : 'TSpObservable',
	#classTraits : 'TSpObservable classTrait',
	#instVars : [
		'#selection => SpObservableSlot',
		'#presenter'
	],
	#category : #'Spec2-Core-Widgets-Tree'
}

{ #category : #'accessing method dictionary' }
SpAbstractTreeSingleSelectionMode class >> on: aPresenter [

	^ self new
		presenter: aPresenter;
		yourself
]

{ #category : #selection }
SpAbstractTreeSingleSelectionMode >> clearSelection [

	selection := #()
]

{ #category : #initialization }
SpAbstractTreeSingleSelectionMode >> initialize [
	self class initializeSlots: self.
	selection := #().
	super initialize.

]

{ #category : #testing }
SpAbstractTreeSingleSelectionMode >> isEmpty [
	
	^ selection isEmpty
]

{ #category : #accessing }
SpAbstractTreeSingleSelectionMode >> isMultipleSelection [
	^ false
]

{ #category : #accesing }
SpAbstractTreeSingleSelectionMode >> presenter: aPresenter [
	presenter := aPresenter
]

{ #category : #selection }
SpAbstractTreeSingleSelectionMode >> selectPath: aPath [

	self subclassResponsibility
]

{ #category : #selection }
SpAbstractTreeSingleSelectionMode >> selectPaths: pathArray [
	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractTreeSingleSelectionMode >> selectedItem [
	^ self subclassResponsibility
]

{ #category : #accessing }
SpAbstractTreeSingleSelectionMode >> selectedPaths [
	^ self subclassResponsibility
]

{ #category : #accessing }
SpAbstractTreeSingleSelectionMode >> selectionHolder [

	^ self observablePropertyNamed: #selection
]

{ #category : #transfert }
SpAbstractTreeSingleSelectionMode >> transferSubscriptionsTo: anotherSelectionMode [

	self selectionHolder transferSubscriptionsTo: anotherSelectionMode selectionHolder
]

{ #category : #selecting }
SpAbstractTreeSingleSelectionMode >> unselectAll [

	self clearSelection
]

{ #category : #selection }
SpAbstractTreeSingleSelectionMode >> unselectPath: aPath [

	self subclassResponsibility
]

{ #category : #'api - events' }
SpAbstractTreeSingleSelectionMode >> whenChangedDo: aBlock [ 
	
	self property: #selection whenChangedDo: aBlock 
]
