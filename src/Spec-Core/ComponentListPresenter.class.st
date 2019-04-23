"
I am a list of components ( Widgets :) )
"
Class {
	#name : #ComponentListPresenter,
	#superclass : #AbstractWidgetPresenter,
	#traits : 'TSpecObservable',
	#classTraits : 'TSpecObservable classTrait',
	#instVars : [
		'#presenters => SpecObservableSlot'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
ComponentListPresenter class >> adapterName [
	^ #ComponentListAdapter
]

{ #category : #initialization }
ComponentListPresenter >> addPresenter: aPresenter [
	presenters add: aPresenter.
	self notifyPropertyChanged: #presenters
]

{ #category : #initialization }
ComponentListPresenter >> initialize [
	self class initializeSlots: self.
	super initialize.
	
	presenters := OrderedCollection new.
	
]

{ #category : #accessor }
ComponentListPresenter >> presenters [
	^ presenters
]
