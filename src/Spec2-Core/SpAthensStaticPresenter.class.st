Class {
	#name : #SpAthensStaticPresenter,
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
SpAthensStaticPresenter class >> adapterName [
	^ #AthensStaticAdapter
]

{ #category : #initialization }
SpAthensStaticPresenter >> drawBlock [
	^ drawBlock
]

{ #category : #initialization }
SpAthensStaticPresenter >> drawBlock: aBlock [
	drawBlock := aBlock.
	
]

{ #category : #initialization }
SpAthensStaticPresenter >> initialize [
	super initialize.
]

{ #category : #initialization }
SpAthensStaticPresenter >> surfaceExtent [
	^ surfaceExtent
]

{ #category : #initialization }
SpAthensStaticPresenter >> surfaceExtent: anExtent [
	surfaceExtent := anExtent.
	
]

{ #category : #'API -Events' }
SpAthensStaticPresenter >> whenDrawBlockChangedDo: aBlockClosure [ 
	self property: #drawBlock whenChangedDo: aBlockClosure 
]

{ #category : #'API -Events' }
SpAthensStaticPresenter >> whenExtentChangedDo: aBlockClosure [ 
	self property: #surfaceExtent whenChangedDo: aBlockClosure 
]
