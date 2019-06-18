Class {
	#name : #MorphPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'#morph => SpecObservableSlot'
	],
	#category : #'Spec-Core-Morphic'
}

{ #category : #specs }
MorphPresenter class >> adapterName [

	^ #MorphAdapter
]

{ #category : #initialization }
MorphPresenter >> defineInputPorts [

	^ { SpecMorphPort new }
]

{ #category : #initialization }
MorphPresenter >> initialize [

	super initialize.
]

{ #category : #accessing }
MorphPresenter >> morph [
	^ morph
]

{ #category : #accessing }
MorphPresenter >> morph: aMorph [

	morph := aMorph
]

{ #category : #'api-events' }
MorphPresenter >> whenMorphChangedDo: aBlock [ 

	self property: #morph whenChangedDo: aBlock
]
