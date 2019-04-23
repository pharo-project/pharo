Class {
	#name : #AthensStaticPresenter,
	#superclass : #AbstractWidgetPresenter,
	#traits : 'TSpecObservable',
	#classTraits : 'TSpecObservable classTrait',
	#instVars : [
		'#drawBlock => SpecObservableSlot',
		'#surfaceExtent => SpecObservableSlot'
	],
	#category : #'Spec-Core-Widgets-Athens'
}

{ #category : #specs }
AthensStaticPresenter class >> adapterName [
	^ #AthensStaticAdapter
]

{ #category : #initialization }
AthensStaticPresenter >> drawBlock [
	^ drawBlock
]

{ #category : #initialization }
AthensStaticPresenter >> drawBlock: aBlock [
	drawBlock := aBlock.
	
]

{ #category : #initialization }
AthensStaticPresenter >> initialize [
	self class initializeSlots: self.
	super initialize.
	
]

{ #category : #initialization }
AthensStaticPresenter >> surfaceExtent [
	^ surfaceExtent
]

{ #category : #initialization }
AthensStaticPresenter >> surfaceExtent: anExtent [
	surfaceExtent := anExtent.
	
]

{ #category : #'API -Events' }
AthensStaticPresenter >> whenDrawBlockChangedDo: aBlockClosure [ 
	self property: #drawBlock whenChangedDo: aBlockClosure 
]

{ #category : #'API -Events' }
AthensStaticPresenter >> whenExtentChangedDo: aBlockClosure [ 
	self property: #surfaceExtent whenChangedDo: aBlockClosure 
]
