"
A presenter to display a toggle button.
A toggle button is a button that can be activated (toggled) or deactivated (untoggled).

"
Class {
	#name : #SpToggleButtonPresenter,
	#superclass : #SpAbstractFormButtonPresenter,
	#instVars : [
		'#action => SpObservableSlot',
		'#icon => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpToggleButtonPresenter class >> adapterName [
	
	^ #ToggleButtonAdapter
]

{ #category : #documentation }
SpToggleButtonPresenter class >> documentFactoryMethodSelector [

	^ #newToggleButton
]

{ #category : #specs }
SpToggleButtonPresenter class >> title [

	^ 'Toggle Button'
]

{ #category : #api }
SpToggleButtonPresenter >> action [ 
	"Answer the action to execute when button is pressed.
	 See `SpToggleButtonPresenter>>#action:`"
	
	^ action
]

{ #category : #api }
SpToggleButtonPresenter >> action: aBlock [
	"Set the action to execute when button is pressed. 
	 `aBlock` receives zero arguments."
	
	action := aBlock
]

{ #category : #api }
SpToggleButtonPresenter >> icon [
	"Answer the icon (an instance of `Form`) to be shown with the button."

	^ icon
]

{ #category : #api }
SpToggleButtonPresenter >> icon: anIcon [
	"Set the icon (an instance of `Form`) to be shown with the button."

	icon := anIcon
]

{ #category : #initialization }
SpToggleButtonPresenter >> initialize [

	super initialize.
	action := [ ]
]

{ #category : #'api-events' }
SpToggleButtonPresenter >> whenIconChangedDo: aBlock [
	"Inform when icon property has changed. 
	 `aBlock` has three optional arguments: 
	 - new value
	 - old value
	 - the announcement triggering this action"

	self property: #icon whenChangedDo: aBlock
]
