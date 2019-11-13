"
I am an elementary Spec Widget managing a slider and an input.

Self example

Between the bounds min and max of the slider, I ensure that the value of the input on the accept and the value of the slider are the same.

Note
====
- min and max must be set before use me.
- if autoAccept is true, the character '-' could create problems
"
Class {
	#name : #SpSliderInput,
	#superclass : #SpPresenter,
	#instVars : [
		'slider',
		'input'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpSliderInput class >> defaultSpec [
	^ self sliderLeft
]

{ #category : #specs }
SpSliderInput class >> sliderBottom [
	<spec>
	^ SpBoxLayout newVertical
		add: #input withConstraints: [ :constraints | constraints height: self buttonHeight ];
		add: #slider;
		yourself
]

{ #category : #specs }
SpSliderInput class >> sliderLeft [
	<spec>
	^ SpPanedLayout newHorizontal
		position: 85 percent;
		add: #slider withConstraints: [ :constraints | constraints beNotResizable ];
		add: #input;
		yourself
]

{ #category : #specs }
SpSliderInput class >> sliderRight [
	<spec>
	^ SpPanedLayout newHorizontal
		position: 15 percent;
		add: #input;
		add: #slider;
		yourself
]

{ #category : #specs }
SpSliderInput class >> sliderTop [
	<spec>
	^ SpBoxLayout newVertical
		add: #slider;
		add: #input withConstraints: [ :constraints | constraints height: self buttonHeight ];
		yourself
]

{ #category : #accessing }
SpSliderInput >> autoAccept [
	^ input autoAccept
]

{ #category : #accessing }
SpSliderInput >> autoAccept: aBoolean [
	
	input autoAccept: aBoolean
]

{ #category : #initialization }
SpSliderInput >> connectPresenters [
	slider
		whenValueChangedDo: [ :sliderValue | 
			| inputValue |
			inputValue := input text asNumber.	" ifNotNil: [ :text | text asNumber ]"
			sliderValue == inputValue
				ifFalse: [ input text: sliderValue asString ] ].
	input
		whenTextChangedDo: [ :text | 
			| inputValue |
			inputValue := text asNumber.
			slider value == inputValue
				ifFalse: [ slider value: inputValue ] ]
]

{ #category : #initialization }
SpSliderInput >> initializePresenters [
	slider := self instantiate: SpSliderPresenter.
	input := self instantiate: SpTextInputFieldPresenter.
	self focusOrder add: input
]

{ #category : #accessing }
SpSliderInput >> input [
	^ input
]

{ #category : #accessing }
SpSliderInput >> label [
	^ slider label
]

{ #category : #accessing }
SpSliderInput >> label: aString [
	slider label: aString
]

{ #category : #accessing }
SpSliderInput >> max: aNumber [
	slider max: aNumber
]

{ #category : #accessing }
SpSliderInput >> min: aNumber [
	slider min: aNumber.
	input text: aNumber asString 
]

{ #category : #accessing }
SpSliderInput >> slider [
	^ slider
]

{ #category : #accessing }
SpSliderInput >> value [
	^ slider value
]

{ #category : #accessing }
SpSliderInput >> value: aNumber [
	slider value: aNumber 
]

{ #category : #'api-events' }
SpSliderInput >> whenValueChangedDo: aBlock [
	slider whenValueChangedDo: aBlock
]
