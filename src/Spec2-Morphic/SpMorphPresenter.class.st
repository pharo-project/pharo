"
A presenter to display a morph.
Sometimes, an user may want to plug a custom morph into Spec2. 
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

{ #category : #documentation }
SpMorphPresenter class >> documentExamplesProtocol [
	
	^ #'*Spec2-Morphic-Examples'
]

{ #category : #documentation }
SpMorphPresenter class >> documentFactoryMethodSelector [

	^ #newMorph
]

{ #category : #documentation }
SpMorphPresenter class >> documentTransmissionsProtocol [

	^ #'transmission'
]

{ #category : #transmission }
SpMorphPresenter >> defaultInputPort [

	^ self inputMorphPort
]

{ #category : #transmission }
SpMorphPresenter >> inputMorphPort [ 

	^ self class environment 
		at: #SpMorphPort
		ifPresent: [ :aClass | aClass newPresenter: self ]
		ifAbsent: [ nil ]
]

{ #category : #api }
SpMorphPresenter >> morph [
	"Answer the morph that will be plugged into the presenter."

	^ morph
]

{ #category : #api }
SpMorphPresenter >> morph: aMorph [
	"Set the morph that will be plugged into the presenter."

	morph := aMorph
]

{ #category : #'api-events' }
SpMorphPresenter >> whenMorphChangedDo: aBlock [ 
	"Inform when morph has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #morph whenChangedDo: aBlock
]
