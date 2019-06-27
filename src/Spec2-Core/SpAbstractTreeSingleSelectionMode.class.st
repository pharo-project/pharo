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

{ #category : #selection }
SpAbstractTreeSingleSelectionMode >> replaceOtherSelectionMode: anotherSelection [

	self subclassResponsibility 
]

{ #category : #selection }
SpAbstractTreeSingleSelectionMode >> selectPath: aPath [

	self subclassResponsibility
]

{ #category : #selection }
SpAbstractTreeSingleSelectionMode >> selectPaths: pathArray [
	self subclassResponsibility
]

{ #category : #selecting }
SpAbstractTreeSingleSelectionMode >> unselectAll [

	self clearSelection
]

{ #category : #'API - events' }
SpAbstractTreeSingleSelectionMode >> whenChangedDo: aBlockClosure [ 
	
	self property: #selection whenChangedDo: aBlockClosure 
]
