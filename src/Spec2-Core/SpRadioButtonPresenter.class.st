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
	#name : #SpRadioButtonPresenter,
	#superclass : #SpAbstractFormButtonPresenter,
	#instVars : [
		'associatedRadioButtons',
		'initialStateSet'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpRadioButtonPresenter class >> adapterName [

	^ #RadioButtonAdapter
]

{ #category : #specs }
SpRadioButtonPresenter class >> title [

	^ 'Radio Button'
]

{ #category : #accessing }
SpRadioButtonPresenter >> associatedRadioButtons [
	^ associatedRadioButtons
]

{ #category : #'associated buttons' }
SpRadioButtonPresenter >> associatedRadioButtons: aCollection [ 
	| allElements |
	
	"pass group to all members"	
	allElements := { self }, aCollection.
	allElements do: [ :each | 
		each basicAssociatedRadioButtons: allElements ].
	
	"self state: true."
	aCollection do: [ :each | each state: false ]
]

{ #category : #private }
SpRadioButtonPresenter >> basicAssociatedRadioButtons: aCollection [ 
	
	associatedRadioButtons := aCollection copyWithout: self
]

{ #category : #accessing }
SpRadioButtonPresenter >> initialStateNotSet [
	"Setting this value to the first radio button will cause the entire group to be unset 
	 on first display. This will be ignored if sent to other than the own containing the 
	 associated buttons"

	initialStateSet := false
]

{ #category : #initialization }
SpRadioButtonPresenter >> initialize [

	super initialize.
	state := true.
	initialStateSet := true.
	associatedRadioButtons := #()
]

{ #category : #testing }
SpRadioButtonPresenter >> isInitialStateSet [

	^ initialStateSet
]

{ #category : #accessing }
SpRadioButtonPresenter >> state: aValue [

	(aValue not 
		and: [ associatedRadioButtons noneSatisfy: [ :e | e state ] ])
		ifTrue: [ 
			"If I am the only one in true, I cannot be unselected" 
			^ self ].

	super state: aValue.
	
	aValue ifTrue: [ 
		associatedRadioButtons 
			do: [ :e | e state: false ] ].
]
