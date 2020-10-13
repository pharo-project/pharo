"
A presenter to create a menu button (a button who exposes a menu instead having an action) 

"
Class {
	#name : #SpMenuButtonPresenter,
	#superclass : #SpAbstractButtonPresenter,
	#instVars : [
		'#menu => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpMenuButtonPresenter class >> adapterName [

	^ #MenuButtonAdapter
]

{ #category : #documentation }
SpMenuButtonPresenter class >> documentFactoryMethodSelector [

	^ #newMenuButton
]

{ #category : #specs }
SpMenuButtonPresenter class >> title [

	^ 'MenuButton'
]

{ #category : #initialization }
SpMenuButtonPresenter >> initialize [
	super initialize.
]

{ #category : #api }
SpMenuButtonPresenter >> menu [
	"Answer the menu (an instance of `SpMenuPresenter` or a block) to show."

	^ menu
]

{ #category : #api }
SpMenuButtonPresenter >> menu: aValuable [
	"Set the menu to show. 
	 `aValuable` can be an instance of `SpMenuPresenter` or a block to allow dynamic behavior."

	menu := aValuable
]

{ #category : #'api-events' }
SpMenuButtonPresenter >> whenMenuChangedDo: aBlock [
	"Inform when menu  property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #menu whenChangedDo: aBlock
]
