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
		'#state => SpObservableSlot',
		'#label => SpObservableSlot',
		'#actionWhenActivated => SpObservableSlot',
		'#actionWhenDeactivated => SpObservableSlot',
		'#labelClickable => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #testing }
SpAbstractFormButtonPresenter class >> isAbstract [
	^ self = SpAbstractFormButtonPresenter
]

{ #category : #api }
SpAbstractFormButtonPresenter >> activationAction: aBlock [
	"This method is used to set the action to perform when I am activated"

	actionWhenActivated := aBlock
]

{ #category : #api }
SpAbstractFormButtonPresenter >> click [
	"Simulate a click on the checkbox
	Used when the checkboc is a list item"

	self toggleState
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> deactivationAction: aBlock [
	"This method is used to set the action to perform when I am deactivated"

	actionWhenDeactivated := aBlock
]

{ #category : #api }
SpAbstractFormButtonPresenter >> enabled: aBoolean [
	"Set if the button is enabled (clickable)"

	super enabled: aBoolean.
	self changed: #enabled.

]

{ #category : #initialization }
SpAbstractFormButtonPresenter >> initialize [
	super initialize.

	actionWhenActivated := [  ].
	actionWhenDeactivated := [  ].
	state := false.

	labelClickable := true.
	label := ''.
	self whenLabelClickableChangedDo: [ :aBoolean | self changed: {#labelClickable: . aBoolean} ]
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
	"Return true if the label can be clicked to select the checkbox"

	^ labelClickable
]

{ #category : #api }
SpAbstractFormButtonPresenter >> labelClickable: aBoolean [
	"Set if the label can be clicked to select the checkbox"

	labelClickable := aBoolean
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

	self property: #actionWhenActivated whenChangedDo: aBlock
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

	self property: #actionWhenDeactivated whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenLabelChangedDo: aBlock [

	self property: #label whenChangedDo: aBlock 
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenLabelClickableChangedDo: aBlock [
	"A block performed when the label click is activated or descativated"

	self property: #labelClickable whenChangedDo: aBlock
]
