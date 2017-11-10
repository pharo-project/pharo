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
	#name : #SliderInput,
	#superclass : #ComposablePresenter,
	#instVars : [
		'slider',
		'input'
	],
	#category : #Spec-PolyWidgets-Elementary
}

{ #category : #example }
SliderInput class >> example [
	| example |
	example := self new
		min: 0;
		max: 250;
		autoAccept: true;
		value: 120;
		extent: 400 @ 50;
		title: 'SliderInput example';
		openWithSpec.
	^ example
]

{ #category : #specs }
SliderInput class >> sliderBottom [
	<spec>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #input origin: 0 @ 0 corner: 1 @ 0.15;
				add: #slider origin: 0 @ 0.15 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SliderInput class >> sliderLeft [
	<spec: #default>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #slider origin: 0 @ 0 corner: 0.85 @ 1;
				add: #input origin: 0.85 @ 0 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SliderInput class >> sliderRight [
	<spec>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #input origin: 0 @ 0 corner: 0.15 @ 1;
				add: #slider origin: 0.15 @ 0 corner: 1 @ 1 ];
		yourself
]

{ #category : #specs }
SliderInput class >> sliderTop [
	<spec>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #slider origin: 0 @ 0 corner: 1 @ 0.85;
				add: #input origin: 0 @ 0.85 corner: 1 @ 1 ];
		yourself
]

{ #category : #accessing }
SliderInput >> autoAccept [
	^ input autoAccept
]

{ #category : #accessing }
SliderInput >> autoAccept: aBoolean [
	
	input autoAccept: aBoolean
]

{ #category : #initialization }
SliderInput >> initializePresenter [
	slider
		whenValueChangedDo: [ :sliderValue | 
			| inputValue |
			inputValue := input text asNumber. " ifNotNil: [ :text | text asNumber ]"
			sliderValue == inputValue
				ifFalse: [ input text: sliderValue asString ] ].
	input
		whenTextChanged: [ :text | 
			| inputValue |
			inputValue := text asNumber.
			slider value == inputValue
				ifFalse: [ slider value: inputValue ] ]
]

{ #category : #initialization }
SliderInput >> initializeWidgets [
	slider := self instantiate: SliderPresenter.
	input := self instantiate: TextInputFieldPresenter.
	self focusOrder add: input
]

{ #category : #accessing }
SliderInput >> input [
	^ input
]

{ #category : #accessing }
SliderInput >> label [
	^ slider label
]

{ #category : #accessing }
SliderInput >> label: aString [
	slider label: aString
]

{ #category : #accessing }
SliderInput >> max: aNumber [
	slider max: aNumber
]

{ #category : #accessing }
SliderInput >> min: aNumber [
	slider min: aNumber.
	input text: aNumber asString 
]

{ #category : #accessing }
SliderInput >> slider [
	^ slider
]

{ #category : #accessing }
SliderInput >> value [
	^ slider value
]

{ #category : #accessing }
SliderInput >> value: aNumber [
	slider value: aNumber 
]

{ #category : #api-events }
SliderInput >> whenValueChangedDo: aBlock [
	slider whenValueChangedDo: aBlock
]
