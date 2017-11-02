"
Model used for radio buttons in example of basic controls (see ""UITheme exampleBasicControls"").
"
Class {
	#name : #ExampleRadioButtonModel,
	#superclass : #Model,
	#instVars : [
		'option'
	],
	#category : 'Morphic-Examples-Widgets'
}

{ #category : #'as yet unclassified' }
ExampleRadioButtonModel >> beCenter [
	"Set the option to #center."

	self option: #center
]

{ #category : #'as yet unclassified' }
ExampleRadioButtonModel >> beLeft [
	"Set the option to #left."

	self option: #left
]

{ #category : #'as yet unclassified' }
ExampleRadioButtonModel >> beRight [
	"Set the option to #right."

	self option: #right
]

{ #category : #initialization }
ExampleRadioButtonModel >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		option: #left
]

{ #category : #'as yet unclassified' }
ExampleRadioButtonModel >> isCenter [
	"Answer whether the option if #center."

	^self option == #center
]

{ #category : #'as yet unclassified' }
ExampleRadioButtonModel >> isLeft [
	"Answer whether the option if #left."

	^self option == #left
]

{ #category : #'as yet unclassified' }
ExampleRadioButtonModel >> isRight [
	"Answer whether the option if #right."

	^self option == #right
]

{ #category : #accessing }
ExampleRadioButtonModel >> option [
	"Answer the value of option"

	^ option
]

{ #category : #accessing }
ExampleRadioButtonModel >> option: aSymbol [
	"Set the value of option"

	option := aSymbol.
	self
		changed: #isLeft;
		changed: #isCenter;
		changed: #isRight
]
