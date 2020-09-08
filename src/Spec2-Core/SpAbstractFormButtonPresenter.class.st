"
A base for _form control buttons_ (like radiobuttons or checkboxes).

## Interesting API methods

- `SpAbstractFormButtonPresenter>>#enabled:`
- `SpAbstractFormButtonPresenter>>#label`
- `SpAbstractFormButtonPresenter>>#label:`
- `SpAbstractFormButtonPresenter>>#state`
- `SpAbstractFormButtonPresenter>>#state:`
- `SpAbstractFormButtonPresenter>>#toggleState`

## Events

-  `SpAbstractFormButtonPresenter>>#whenActivatedDo:`
-  `SpAbstractFormButtonPresenter>>#whenChangedDo:`
-  `SpAbstractFormButtonPresenter>>#whenDeactivatedDo:`
-  `SpAbstractFormButtonPresenter>>#whenLabelChangedDo:`
-  `SpAbstractFormButtonPresenter>>#whenLabelClickableChangedDo:`


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

{ #category : #TOREMOVE }
SpAbstractFormButtonPresenter >> activationAction: aBlock [
	"This method is used to set the action to perform when I am activated"

	actionWhenActivated := aBlock
]

{ #category : #simulating }
SpAbstractFormButtonPresenter >> click [
	"Simulate a click on the checkbox
	Used when the checkbox is a list item"

	self toggleState
]

{ #category : #TOREMOVE }
SpAbstractFormButtonPresenter >> deactivationAction: aBlock [
	"This method is used to set the action to perform when I am deactivated"

	actionWhenDeactivated := aBlock
]

{ #category : #api }
SpAbstractFormButtonPresenter >> enabled: aBoolean [
	"Set if the button is enabled."

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
	"Answers the label to be shown by the button"

	^ label
]

{ #category : #api }
SpAbstractFormButtonPresenter >> label: aString [
	"Sets the label to be shown by the button."

	label := aString
]

{ #category : #api }
SpAbstractFormButtonPresenter >> labelClickable [
	"Answer if the label can be clicked to select the control button"

	^ labelClickable
]

{ #category : #api }
SpAbstractFormButtonPresenter >> labelClickable: aBoolean [
	"Set if the label can be clicked to select the control button"

	labelClickable := aBoolean
]

{ #category : #localization }
SpAbstractFormButtonPresenter >> localeChanged [

	super localeChanged.
	self forceUpdateSlot: #label
	
]

{ #category : #api }
SpAbstractFormButtonPresenter >> state [
	"Answer the current state of the control button"

	^ state
]

{ #category : #api }
SpAbstractFormButtonPresenter >> state: aBoolean [
	"Set the state of the control button."

	self state = aBoolean ifTrue: [
		"Do not update in case it does not change"
		^ self ].
	state := aBoolean
]

{ #category : #api }
SpAbstractFormButtonPresenter >> toggleState [
	"Toogle the current state of the control button"

	self state: self state not
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenActivatedDo: aBlock [
	"Informs when the control button has been activated (see `SpAbstractFormButtonPresenter>>#state:`.
	 `aBlock` receives zero arguments." 
	
	self whenChangedDo: [ :aBoolean | aBoolean ifTrue: aBlock ]
]

{ #category : #TOREMOVE }
SpAbstractFormButtonPresenter >> whenActivationActionChangedDo: aBlock [
	"A block performed when the activation action changed"

	self property: #actionWhenActivated whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenChangedDo: aBlock [
	"Informs when the control button state has been changed (see `SpAbstractFormButtonPresenter>>#state:`.
	 `aBlock` receives one argument: 
	 - `aBoolean`, indicating control state." 
	
	self property: #state whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenDeactivatedDo: aBlock [
	"Informs when the control button has been deactivated (see `SpAbstractFormButtonPresenter>>#state:`.
	 `aBlock` receives zero arguments." 
	
	self whenChangedDo: [:bool | bool ifFalse: aBlock ]
]

{ #category : #TOREMOVE }
SpAbstractFormButtonPresenter >> whenDeactivationActionChangedDo: aBlock [
	"A block performed when the deactivation action changed"

	self property: #actionWhenDeactivated whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenLabelChangedDo: aBlock [
	"Informs when the control button label been changed (see `SpAbstractFormButtonPresenter>>#label:`.
	 `aBlock` receives zero arguments." 

	self property: #label whenChangedDo: aBlock 
]

{ #category : #'api-events' }
SpAbstractFormButtonPresenter >> whenLabelClickableChangedDo: aBlock [
	"Informs when the control button label clickable state has been changed (see `SpAbstractFormButtonPresenter>>#labelClickable:`.
	 `aBlock` receives zero arguments." 

	self property: #labelClickable whenChangedDo: aBlock
]
