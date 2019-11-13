Class {
	#name : #SpMorphPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#morph => SpObservableSlot'
	],
	#category : #'Spec2-Core-Morphic'
}

{ #category : #specs }
SpMorphPresenter class >> adapterName [

	^ #MorphAdapter
]

{ #category : #initialization }
SpMorphPresenter >> defineInputPorts [

	^ { SpMorphPort new }
]

{ #category : #initialization }
SpMorphPresenter >> initialize [
	super initialize.
]

{ #category : #accessing }
SpMorphPresenter >> morph [
	^ morph
]

{ #category : #accessing }
SpMorphPresenter >> morph: aMorph [

	morph := aMorph
]

{ #category : #'api-events' }
SpMorphPresenter >> whenMorphChangedDo: aBlock [ 

	self property: #morph whenChangedDo: aBlock
]
