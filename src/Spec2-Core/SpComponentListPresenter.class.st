"
I am a list of components ( Widgets :) )
"
Class {
	#name : #SpComponentListPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#presenters => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpComponentListPresenter class >> adapterName [
	^ #ComponentListAdapter
]

{ #category : #initialization }
SpComponentListPresenter >> addPresenter: aPresenter [
	presenters add: aPresenter.
	self notifyPropertyChanged: #presenters
]

{ #category : #initialization }
SpComponentListPresenter >> initialize [

	super initialize.
	presenters := OrderedCollection new
]

{ #category : #accessor }
SpComponentListPresenter >> presenters [
	^ presenters
]

{ #category : #initialization }
SpComponentListPresenter >> presenters: aCollection [
	presenters := aCollection
]
