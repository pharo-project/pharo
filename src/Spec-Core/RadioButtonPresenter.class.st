"
A RadioButtonPresenter is a spec presenter for RadioButton.
See AbstractFormButtonPresenter

self example

! The method privateSetState: is really private and should only be used by RadioButtonGroup or RadioButtonGroupPresenter !

You can also have a look at RadioButtonGroupExample for a full example of how to use them with a group.

I provide more accessors on my super class ValueHolder
- actionWhenActivatedHolder
- actionWhenDeactivatedHolder
- enabledHolder
- stateHolder

I provide the variable canDeselectByClick and its accessors, it is a boolean representing if the radio can be deselect directly by the user using a click on it.

I specialize state: to use the variable canDeselectByClick.
"
Class {
	#name : #RadioButtonPresenter,
	#superclass : #AbstractFormButtonPresenter,
	#instVars : [
		'canDeselectByClick'
	],
	#category : 'Spec-Core-Widgets'
}

{ #category : #specs }
RadioButtonPresenter class >> adapterName [

	^ #RadioButtonAdapter
]

{ #category : #specs }
RadioButtonPresenter class >> defaultSpec [
	<spec: #default>
	
	^ #(RadioButtonAdapter
		adapt: #(model))
]

{ #category : #specs }
RadioButtonPresenter class >> title [

	^ 'Radio Button'
]

{ #category : #accessing }
RadioButtonPresenter >> actionWhenActivatedHolder [
	
	^ actionWhenActivatedHolder
]

{ #category : #accessing }
RadioButtonPresenter >> actionWhenDeactivatedHolder [
	
	^ actionWhenDeactivatedHolder
]

{ #category : #protocol }
RadioButtonPresenter >> canDeselectByClick [
	<api: #inspect>
	"Return true clicking on a selected radio button deselects it"

	^ canDeselectByClick value
]

{ #category : #protocol }
RadioButtonPresenter >> canDeselectByClick: aBoolean [
	<api: #boolean getter: #canDeselectByClick registration: #whenCanDeselectByClickChanged>
	"Set if clicking on a selected radio button can deselect it"
	
	canDeselectByClick value: aBoolean
]

{ #category : #api }
RadioButtonPresenter >> enabled: aBoolean [
	"<api: #boolean getter: #enabled registration: #whenEnabledChanged:>"
	"Set if the button is enabled (clickable)"

	super enabled: aBoolean.
	self changed: #enabled.

]

{ #category : #initialization }
RadioButtonPresenter >> initialize [
	super initialize.
	canDeselectByClick := false asValueHolder.
]

{ #category : #private }
RadioButtonPresenter >> privateSetState: aBoolean [
	"Should only be used by RadioButtonGroup"
	
	stateHolder value: aBoolean.
	self changed: #state
]

{ #category : #protocol }
RadioButtonPresenter >> state: aBoolean [
	"Set if the radio is activated or not"

	<api: #boolean getter: #state>
	self canDeselectByClick
		ifTrue: [ stateHolder value: aBoolean ]
		ifFalse: [ self state
				ifFalse: [ stateHolder value: aBoolean ] ]
]

{ #category : #accessing }
RadioButtonPresenter >> stateHolder [
	
	^ stateHolder
]

{ #category : #protocol-events }
RadioButtonPresenter >> whenCanDeselectByClickChanged: aBlock [
	"This method is triggered when `canDeselectByClick` changes"
	
	canDeselectByClick whenChangedDo: aBlock
]
