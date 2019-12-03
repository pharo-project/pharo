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
		'newPresenterBlock',
		'columnsWillChangeBlock'
	],
	#category : #'Spec2-Core-Miller'
}

{ #category : #accessing }
SpMillerColumnPresenter >> addPresenter: newSubPresenter [

	newSubPresenter whenActivatedDo: [ :selection | 
		self changeSelection: selection selectedItem from: newSubPresenter ].
	newSubPresenter owner: self.
	layout add: newSubPresenter
]

{ #category : #private }
SpMillerColumnPresenter >> changeSelection: selection from: aPresenter [
	| selectedPresenterIndex |

	selectedPresenterIndex := self presenters indexOf: aPresenter.
	columnsWillChangeBlock ifNotNil: [ 
		columnsWillChangeBlock cull: selectedPresenterIndex ].
	self resetTo: selectedPresenterIndex.
	self pushModel: selection
]

{ #category : #initialization }
SpMillerColumnPresenter >> initialize [

	super initialize.
	newPresenterBlock := [ :model | Error signal: 'should provide a new presenter block' ]
]

{ #category : #initialization }
SpMillerColumnPresenter >> initializePresenters [
	
	layout := SpMillerLayout newHorizontal
]

{ #category : #private }
SpMillerColumnPresenter >> newPresenterFor: aModel [

	^ newPresenterBlock value: aModel
]

{ #category : #accessing }
SpMillerColumnPresenter >> pages [
	
	^ layout presenters
]

{ #category : #accessing }
SpMillerColumnPresenter >> presenterBlock: aBlockClosure [ 
	
	newPresenterBlock := aBlockClosure
]

{ #category : #accessing }
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

{ #category : #model }
SpMillerColumnPresenter >> setRootModel: aModel [

	self resetTo: 0.
	self pushModel: aModel
]

{ #category : #accessing }
SpMillerColumnPresenter >> size [

	^ self presenters size
]

{ #category : #accessing }
SpMillerColumnPresenter >> spacing [

	^ layout spacing
]

{ #category : #accessing }
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

{ #category : #accessing }
SpMillerColumnPresenter >> visiblePages [ 

	^ layout visiblePages
]

{ #category : #accessing }
SpMillerColumnPresenter >> visiblePages: aNumber [ 

	layout visiblePages: aNumber
]

{ #category : #'api-events' }
SpMillerColumnPresenter >> whenColumnsWillChangeDo: aBlock [

	columnsWillChangeBlock := aBlock
]
