Class {
	#name : #ButtonBarPresenter,
	#superclass : #AbstractWidgetPresenter,
	#instVars : [
		'items',
		'placeAtStart'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
ButtonBarPresenter class >> adapterName [

	^ #ButtonBarAdapter
]

{ #category : #adding }
ButtonBarPresenter >> add: aButtonPresenter [
	
	aButtonPresenter owner: self.
	items add: aButtonPresenter
]

{ #category : #initialization }
ButtonBarPresenter >> initialize [

	super initialize.
	items := OrderedCollection new asValueHolder.
	self placeAtEnd
]

{ #category : #testing }
ButtonBarPresenter >> isPlaceAtEnd [

	^ self isPlaceAtStart not
]

{ #category : #testing }
ButtonBarPresenter >> isPlaceAtStart [

	^ placeAtStart
]

{ #category : #api }
ButtonBarPresenter >> items [
	^ items value
]

{ #category : #accessing }
ButtonBarPresenter >> placeAtEnd [

	placeAtStart := false
]

{ #category : #accessing }
ButtonBarPresenter >> placeAtStart [ 

	placeAtStart := true
]
