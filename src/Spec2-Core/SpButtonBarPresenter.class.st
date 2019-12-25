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

{ #category : #testing }
SpButtonBarPresenter >> canTakeKeyboardFocus [

	^ false
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

{ #category : #api }
SpButtonBarPresenter >> items: aCollectionOfButtonPresenters [

	items := OrderedCollection new asValueHolder.
	aCollectionOfButtonPresenters 
		do: [ :each | each owner: self ].
	items addAll: aCollectionOfButtonPresenters
]

{ #category : #accessing }
SpButtonBarPresenter >> placeAtEnd [

	placeAtStart := false
]

{ #category : #accessing }
SpButtonBarPresenter >> placeAtStart [ 

	placeAtStart := true
]

{ #category : #'private-traversing' }
SpButtonBarPresenter >> presenters [
	
	^ self items
]
