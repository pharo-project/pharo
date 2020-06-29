"
@@TODO

!! Transmissions
- #inputMorphPort (default)
"
Class {
	#name : #SpMorphPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#morph => SpObservableSlot'
	],
	#category : #'Spec2-Morphic-Widgets'
}

{ #category : #specs }
SpMorphPresenter class >> adapterName [

	^ #MorphAdapter
]

{ #category : #transmission }
SpMorphPresenter >> defaultInputPort [

	^ self inputMorphPort
]

{ #category : #initialization }
SpMorphPresenter >> initialize [
	super initialize.
]

{ #category : #transmission }
SpMorphPresenter >> inputMorphPort [ 

	^ self class environment 
		at: #SpMorphPort
		ifPresent: [ :aClass | aClass newPresenter: self ]
		ifAbsent: [ nil ]
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
