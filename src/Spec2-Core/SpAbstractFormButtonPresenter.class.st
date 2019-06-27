"
AbstractFormButtonPresenter is an abstract class for button of form (like radiobuttons or checkbox).
See AbstractWidgetPresenter

self example

I provide the following variables and their accessors
- activationAction and desactivationAction are actions to perform when I am activeted / desactivated.
- label is the text displayed near the box.
- state is a boolean representing if I am activated, it is false by default

I provide the following methods
- click to simulate a click on me.
- toggleState to reverse my state.

"
Class {
	#name : #SpAbstractFormButtonPresenter,
	#superclass : #SpAbstractWidgetPresenter,
	#instVars : [
		'#actionWhenActivatedHolder',
		'#state => SpObservableSlot',
		'#labelClickableHolder',
		'#label => SpObservableSlot',
		'#actionWhenDeactivatedHolder'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #api }
SpAbstractFormButtonPresenter >> activationAction: aBlock [
	"<api: #block getter: #activationAction registration: #whenActivationActionChanged:>"
	"This method is used to set the action to perform when I am activated"
	
	actionWhenActivatedHolder value: aBlock
]

{ #category : #api }
SpAbstractFormButtonPresenter >> click [
	"<api: #do>"
	"Simulate a click on the checkbox
	Used when the checkboc is a list item"

	self toggleState
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> deactivationAction: aBlock [
	"<api: #block getter: #desactivationAction registration: #whenDeactivationActionChanged:>"
	"This method is used to set the action to perform when I am deactivated"
	
	actionWhenDeactivatedHolder value: aBlock
]

{ #category : #api }
SpAbstractFormButtonPresenter >> enabled: aBoolean [
	"<api: #boolean getter: #enabled registration: #whenEnabledChanged:>"
	"Set if the button is enabled (clickable)"

	super enabled: aBoolean.
	self changed: #enabled.

]

{ #category : #initialization }
SpAbstractFormButtonPresenter >> initialize [

	super initialize.

	actionWhenActivatedHolder := [] asValueHolder.
	actionWhenDeactivatedHolder := [] asValueHolder.
	state := false.
	
	labelClickableHolder := true asValueHolder. 
	label := ''.
	labelClickableHolder whenChangedDo: [:aBoolean | self changed: { #labelClickable: . aBoolean } ]
]

{ #category : #api }
SpAbstractFormButtonPresenter >> label [

	^ label
]

{ #category : #api }
SpAbstractFormButtonPresenter >> label: aString [

	label := aString
]

{ #category : #api }
SpAbstractFormButtonPresenter >> labelClickable [
"	<api: #inspect>"
	"Return true if the label can be clicked to select the checkbox"

	^ labelClickableHolder value
]

{ #category : #api }
SpAbstractFormButtonPresenter >> labelClickable: aBoolean [
	"<api: #boolean getter: #labelClickable registration: #whenLabelClickableChanged:>"
	"Set if the label can be clicked to select the checkbox"
	
	labelClickableHolder value: aBoolean
]

{ #category : #api }
SpAbstractFormButtonPresenter >> state [
	"Return the current state of the form button"

	^ state
]

{ #category : #api }
SpAbstractFormButtonPresenter >> state: aBoolean [

	self state = aBoolean
		ifTrue: [
			"Do not update in case it does not change"
			^ self ].
	state := aBoolean
]

{ #category : #api }
SpAbstractFormButtonPresenter >> toggleState [
	"<api: #do>"
	"Toogle the current state of the checkbox"

	self state: self state not
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenActivatedDo: aBlock [
	"This method is used to propagate the event that I have been activated" 
	
	self whenChangedDo: [:bool | bool ifTrue: aBlock ]
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenActivationActionChangedDo: aBlock [
	"A block performed when the activation action changed"

	actionWhenActivatedHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenChangedDo: aBlock [
	"This method is used to propagate the event that I have changed"
	
	self property: #state whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenDeactivatedDo: aBlock [
	"This method is used to propagate the event that I have been deactivated"
	
	self whenChangedDo: [:bool | bool ifFalse: aBlock ]
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenDeactivationActionChangedDo: aBlock [
	"A block performed when the deactivation action changed"

	actionWhenDeactivatedHolder whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenLabelChangedDo: aBlock [

	self property: #label whenChangedDo: aBlock 
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenLabelClickableChangedDo: aBlock [
	"A block performed when the label click is activated or descativated"
	
	labelClickableHolder whenChangedDo:  aBlock 
]
