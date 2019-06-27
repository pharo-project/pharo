Class {
	#name : #SpButtonBarPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'items',
		'placeAtStart'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpButtonBarPresenter class >> adapterName [

	^ #ButtonBarAdapter
]

{ #category : #adding }
SpButtonBarPresenter >> add: aButtonPresenter [
	
	aButtonPresenter owner: self.
	items add: aButtonPresenter
]

{ #category : #initialization }
SpButtonBarPresenter >> initialize [

	super initialize.
	items := OrderedCollection new asValueHolder.
	self placeAtEnd
]

{ #category : #testing }
SpButtonBarPresenter >> isPlaceAtEnd [

	^ self isPlaceAtStart not
]

{ #category : #testing }
SpButtonBarPresenter >> isPlaceAtStart [

	^ placeAtStart
]

{ #category : #api }
SpButtonBarPresenter >> items [
	^ items value
]

{ #category : #accessing }
SpButtonBarPresenter >> placeAtEnd [

	placeAtStart := false
]

{ #category : #accessing }
SpButtonBarPresenter >> placeAtStart [ 

	placeAtStart := true
]
