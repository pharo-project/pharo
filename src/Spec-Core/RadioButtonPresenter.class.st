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
		'associatedRadioButtons'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
RadioButtonPresenter class >> adapterName [

	^ #RadioButtonAdapter
]

{ #category : #specs }
RadioButtonPresenter class >> title [

	^ 'Radio Button'
]

{ #category : #accessing }
RadioButtonPresenter >> associatedRadioButtons [
	^ associatedRadioButtons
]

{ #category : #'associated buttons' }
RadioButtonPresenter >> associatedRadioButtons: aCollection [ 
	
	| allElements |
		
	allElements := { self } , aCollection.
	allElements do: [ :e | e basicAssociatedRadioButtons: allElements ].
	
	self state: true.
	aCollection do: [ :e | e state: false ]
]

{ #category : #private }
RadioButtonPresenter >> basicAssociatedRadioButtons: aCollection [ 
	
	associatedRadioButtons := aCollection copyWithout: self
]

{ #category : #initialization }
RadioButtonPresenter >> initialize [
	super initialize.
	state := true.
	associatedRadioButtons := #()
]

{ #category : #accessing }
RadioButtonPresenter >> state: aValue [

	(associatedRadioButtons noneSatisfy: [ :e | e state ])
		ifTrue: [ 
			"If I am the only one in true, I cannot be unselected" 
			^ self ].

	super state: aValue.
	
	aValue ifTrue: [ 
		associatedRadioButtons 
			do: [ :e | e state: false ]].
]
