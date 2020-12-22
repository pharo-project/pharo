"
A full width bar for arranging buttons.
A button bar can be used to provide a consistent layout of buttons throughout your application. 

"
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

{ #category : #documentation }
SpButtonBarPresenter class >> documentFactoryMethodSelector [

	^ #newButtonBar
]

{ #category : #api }
SpButtonBarPresenter >> add: aButtonPresenter [
	"Add `aButtonPresenter` to the button list."
	
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
	"Answer when the button bar will place buttons at the end of the bar"

	^ self isPlaceAtStart not
]

{ #category : #testing }
SpButtonBarPresenter >> isPlaceAtStart [
	"Answer when the button bar will place buttons at the start of the bar"

	^ placeAtStart
]

{ #category : #api }
SpButtonBarPresenter >> items [
	"Answer list of buttons."

	^ items value
]

{ #category : #api }
SpButtonBarPresenter >> items: aCollectionOfButtonPresenters [
	"Set buttons to show, taking elements in `aCollectionOfButtonPresenters`."

	items := OrderedCollection new asValueHolder.
	aCollectionOfButtonPresenters 
		do: [ :each | each owner: self ].
	items addAll: aCollectionOfButtonPresenters
]

{ #category : #api }
SpButtonBarPresenter >> placeAtEnd [
	"Indicate the button bar will place buttons at the end of the bar (arranged to the right)"

	placeAtStart := false
]

{ #category : #api }
SpButtonBarPresenter >> placeAtStart [ 
	"Indicate the button bar will place buttons at the start of the bar (arranged to the left)"

	placeAtStart := true
]

{ #category : #'private-traversing' }
SpButtonBarPresenter >> presenters [
	
	^ self items
]
