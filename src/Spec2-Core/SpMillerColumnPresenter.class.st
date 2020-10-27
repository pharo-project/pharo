"
I am a miller columns widget, aka cascading lists.
I am composed of presenters flowing to the right.
Every time that one of my inner presenters is activated, I ""dive in"" that element, showing a new presenter for the activated element to the right of the current selection.

You have to set me:
 - presenterBlock: aBlock that receives a model as argument and returns a SpecMillerPresenter or compatible (read below)
 - a first presenter using either
    - setModel: with a model  that calculates the initial presenter using the presenter block if already available
    - setPresenter: with a SpecMillerPresenter or compatible (read below)


SpecMillerColumnPresenter works on SpecMillerPresenter or compatible objects.
All presenters inside this one should be polymorphic, defining:
 - whenActivatedDo:, an event that will invoke a valuable with a selection object (understanding #selectedObject)
"
Class {
	#name : #SpMillerColumnPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'#newPresenterBlock',
		'#columnsWillChangeBlock',
		'#horizontalScrollBar => SpObservableSlot',
		'#columnsChangedBlock'
	],
	#category : #'Spec2-Core-Miller'
}

{ #category : #api }
SpMillerColumnPresenter >> addPresenter: newSubPresenter [

	newSubPresenter whenActivatedDo: [ :selection | 
		self changeSelection: selection selectedItem from: newSubPresenter ].
	newSubPresenter owner: self.
	layout add: newSubPresenter
]

{ #category : #private }
SpMillerColumnPresenter >> announceColumnsChanged: index [

	columnsChangedBlock ifNil: [ ^ self ]. 
	columnsChangedBlock cull: index
]

{ #category : #private }
SpMillerColumnPresenter >> announceColumnsWillChange: index [

	columnsWillChangeBlock ifNil: [ ^ self ]. 
	columnsWillChangeBlock cull: index
]

{ #category : #private }
SpMillerColumnPresenter >> changeSelection: selection from: aPresenter [
	| selectedPresenterIndex |

	selectedPresenterIndex := self presenters indexOf: aPresenter.
	self announceColumnsWillChange: selectedPresenterIndex.
	self resetTo: selectedPresenterIndex.
	self pushModel: selection.
	self announceColumnsChanged: selectedPresenterIndex
]

{ #category : #testing }
SpMillerColumnPresenter >> hasHorizontalScrollBar [

	^ horizontalScrollBar
]

{ #category : #initialization }
SpMillerColumnPresenter >> initialize [

	super initialize.
	newPresenterBlock := [ :model | Error signal: 'should provide a new presenter block' ].
	self withHorizontalScrollBar
]

{ #category : #initialization }
SpMillerColumnPresenter >> initializePresenters [
	
	layout := SpMillerLayout newHorizontal
]

{ #category : #private }
SpMillerColumnPresenter >> newPresenterFor: aModel [

	^ newPresenterBlock value: aModel
]

{ #category : #api }
SpMillerColumnPresenter >> pages [
	
	^ layout presenters
]

{ #category : #api }
SpMillerColumnPresenter >> presenterBlock: aBlock [ 
	
	newPresenterBlock := aBlock
]

{ #category : #api }
SpMillerColumnPresenter >> presenters [

	^ layout presenters
]

{ #category : #model }
SpMillerColumnPresenter >> pushModel: aModel [

	self addPresenter: (self newPresenterFor: aModel)
]

{ #category : #private }
SpMillerColumnPresenter >> resetTo: anIndex [
	"Remove all presenters up to anIndex.
	0 means to remove all elements."
	self presenters copy withIndexDo: [ :presenter :index |
		index <= anIndex ifFalse: [ layout remove: presenter ] ]
]

{ #category : #paginator }
SpMillerColumnPresenter >> scrollByDeltaPercent: percent [
	
	self withAdapterDo: [ :anAdapter | 
		anAdapter scrollByDeltaPercent: percent ]
]

{ #category : #api }
SpMillerColumnPresenter >> selectPage: aNumber [

	self withAdapterDo: [ :anAdapter |
		anAdapter selectPage: aNumber ]
]

{ #category : #model }
SpMillerColumnPresenter >> setRootModel: aModel [

	self resetTo: 0.
	self pushModel: aModel
]

{ #category : #api }
SpMillerColumnPresenter >> size [

	^ self presenters size
]

{ #category : #api }
SpMillerColumnPresenter >> spacing [

	^ layout spacing
]

{ #category : #api }
SpMillerColumnPresenter >> spacing: aNumber [

	layout spacing: aNumber
]

{ #category : #'private-traversing' }
SpMillerColumnPresenter >> traverseInFocusOrderDo: aBlock excluding: excludes [

	self pages do: [ :each | 
		each 
			traverseInFocusOrderDo: aBlock 
			excluding: excludes ]
]

{ #category : #api }
SpMillerColumnPresenter >> visiblePages [

	^ layout visiblePages
]

{ #category : #api }
SpMillerColumnPresenter >> visiblePages: aNumber [ 

	layout visiblePages: aNumber
]

{ #category : #'api-events' }
SpMillerColumnPresenter >> whenColumnsChangedDo: aBlock [

	columnsChangedBlock := aBlock
]

{ #category : #'api-events' }
SpMillerColumnPresenter >> whenColumnsWillChangeDo: aBlock [

	columnsWillChangeBlock := aBlock
]

{ #category : #'api-events' }
SpMillerColumnPresenter >> whenHorizontalScrollBarChangedDo: aBlock [

	self property: #horizontalScrollBar whenChangedDo: aBlock
]

{ #category : #api }
SpMillerColumnPresenter >> withHorizontalScrollBar [

	horizontalScrollBar := true
]

{ #category : #api }
SpMillerColumnPresenter >> withoutHorizontalScrollBar [

	horizontalScrollBar := false
]
