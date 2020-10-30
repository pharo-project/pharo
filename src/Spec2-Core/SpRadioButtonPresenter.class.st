"
A presenter for Radio Buttons.
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

{ #category : #documentation }
SpRadioButtonPresenter class >> documentFactoryMethodSelector [

	^ #newRadioButton
]

{ #category : #specs }
SpRadioButtonPresenter class >> title [

	^ 'Radio Button'
]

{ #category : #api }
SpRadioButtonPresenter >> associatedRadioButtons [
	"Answer the collection of radio buttons associated (grouped) to this one.
	 See also `SpRadioButtonPresenter>>#associatedRadioButtons:`"

	^ associatedRadioButtons
]

{ #category : #api }
SpRadioButtonPresenter >> associatedRadioButtons: aCollection [ 
	"Set the list of radio buttons associated with this one.
	 By using this, you will be grouping together this radio button along with the ones contained 
	 in `aCollection`.
	 You should NOT send this message to all associated radio buttons, the first of them is enough."
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

{ #category : #api }
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
	"Answer if the radio button and its associates start the display with one of the buttons 
	 set. This is often used when you do not have a default option and you need to require the 
	 user to expressely pick one choice."

	^ initialStateSet
]

{ #category : #api }
SpRadioButtonPresenter >> state: aValue [
	"Set the state `aBoolean` of current radio button. 
	 Changing the state affect other associated radio buttons by toggling also their state.
	 See also `SpRadioButtonPresenter>>#associatedRadioButtons:`"

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
