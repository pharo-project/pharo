"
I am a list of components ( Widgets :) )
"
Class {
	#name : #SpComponentListPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#presenters => SpObservableSlot',
		'#presentersChangedBlock'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpComponentListPresenter class >> adapterName [
	^ #ComponentListAdapter
]

{ #category : #initialization }
SpComponentListPresenter >> addPresenter: aPresenter [
	
	aPresenter owner: self.
	presenters add: aPresenter.
	self notifyPropertyChanged: #presenters
]

{ #category : #testing }
SpComponentListPresenter >> includes: aPresenter [ 
	^ presenters includes: aPresenter  
]

{ #category : #initialization }
SpComponentListPresenter >> initialize [

	super initialize.
	presenters := OrderedCollection new
]

{ #category : #testing }
SpComponentListPresenter >> isEmpty [
	^ presenters isEmpty
]

{ #category : #accessor }
SpComponentListPresenter >> presenters [
	^ presenters
]

{ #category : #initialization }
SpComponentListPresenter >> presenters: aCollection [

	aCollection do: [ :each | each owner: self ].
	presenters := aCollection
]

{ #category : #event }
SpComponentListPresenter >> whenPresentersChangedDo: aBlockClosure [ 
	
	self property: #presenters whenChangedDo: aBlockClosure 
]
