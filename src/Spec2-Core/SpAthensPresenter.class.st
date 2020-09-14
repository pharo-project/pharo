"
A presenter that contains an athens surface (See `AthensSurface` and `AthensSurfaceExamples class`). 
Users can draw their own vectorial images  and render them as part of a Spec based application by using this presenter.

"
Class {
	#name : #SpAthensPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#traits : 'SpTContextMenu',
	#classTraits : 'SpTContextMenu classTrait',
	#instVars : [
		'#drawBlock => SpObservableSlot',
		'#surfaceExtent => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Athens'
}

{ #category : #specs }
SpAthensPresenter class >> adapterName [
	^ #AthensAdapter
]

{ #category : #documentation }
SpAthensPresenter class >> documentFactoryMethodSelector [

	^ #newAthens
]

{ #category : #api }
SpAthensPresenter >> drawBlock [
	"Answer the block used to draw into an athens canvas."

	^ drawBlock
]

{ #category : #api }
SpAthensPresenter >> drawBlock: aBlock [
	"Set the block used to draw into the athens canvas.
	 `aBlock` receives one argument, the `AthensCanvas` instance used to draw the surface.
	 (See `SpAthensPresenter class>>#example` as an example of usage)"

	drawBlock := aBlock
]

{ #category : #initialization }
SpAthensPresenter >> initialize [
	super initialize.
]

{ #category : #api }
SpAthensPresenter >> surfaceExtent [
	"Anwer the surface extent (the size the surface will have)"

	^ surfaceExtent
]

{ #category : #api }
SpAthensPresenter >> surfaceExtent: anExtent [
	"Set surface extent (a point) to contain the athens drawing."

	surfaceExtent := anExtent
]

{ #category : #'api-events' }
SpAthensPresenter >> whenDrawBlockChangedDo: aBlock [

	self property: #drawBlock whenChangedDo: aBlock 
]

{ #category : #'api-events' }
SpAthensPresenter >> whenExtentChangedDo: aBlock [

	self property: #surfaceExtent whenChangedDo: aBlock 
]
