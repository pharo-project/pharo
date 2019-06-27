Class {
	#name : #AbstractTreeSingleSelectionMode,
	#superclass : #Object,
	#traits : 'TSpecObservable',
	#classTraits : 'TSpecObservable classTrait',
	#instVars : [
		'#selection => SpecObservableSlot',
		'#presenter'
	],
	#category : #'Spec-Core-Widgets-Tree'
}

{ #category : #'accessing method dictionary' }
AbstractTreeSingleSelectionMode class >> on: aPresenter [

	^ self new
		presenter: aPresenter;
		yourself
]

{ #category : #selection }
AbstractTreeSingleSelectionMode >> clearSelection [

	selection := #()
]

{ #category : #initialization }
AbstractTreeSingleSelectionMode >> initialize [
	self class initializeSlots: self.
	selection := #().
	super initialize.

]

{ #category : #testing }
AbstractTreeSingleSelectionMode >> isEmpty [
	
	^ selection isEmpty
]

{ #category : #selection }
AbstractTreeSingleSelectionMode >> replaceOtherSelectionMode: anotherSelection [

	self subclassResponsibility 
]

{ #category : #selection }
AbstractTreeSingleSelectionMode >> selectPath: aPath [

	self subclassResponsibility
]

{ #category : #selection }
AbstractTreeSingleSelectionMode >> selectPaths: pathArray [
	self subclassResponsibility
]

{ #category : #selecting }
AbstractTreeSingleSelectionMode >> unselectAll [

	self clearSelection
]

{ #category : #'API - events' }
AbstractTreeSingleSelectionMode >> whenChangedDo: aBlockClosure [ 
	
	self property: #selection whenChangedDo: aBlockClosure 
]
