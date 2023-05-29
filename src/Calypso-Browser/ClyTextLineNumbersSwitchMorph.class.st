"
I am a clickable label to toggle text morph decoration with line numbers.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	label:		<StringMorph>
"
Class {
	#name : #ClyTextLineNumbersSwitchMorph,
	#superclass : #ClyTextMorphToolMorph,
	#instVars : [
		'label'
	],
	#classInstVars : [
		'showLineNumbers'
	],
	#category : #'Calypso-Browser-TextEditors'
}

{ #category : #settings }
ClyTextLineNumbersSwitchMorph class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #showLineNumbers)
		label: 'Show lines numbers for all browsers';
		parent: #Calypso;
		default: false;
		target: self;
		description: 'If true, line numbers will appear in new system browsers for method editor'
]

{ #category : #settings }
ClyTextLineNumbersSwitchMorph class >> showLineNumbers [
	^ showLineNumbers ifNil: [ false ]
]

{ #category : #settings }
ClyTextLineNumbersSwitchMorph class >> showLineNumbers: aBoolean [
	showLineNumbers := aBoolean.
	self allInstancesDo: [ :inst |
		| textMorph |
		textMorph := inst textMorph.
		(showLineNumbers and: [textMorph lineNumbersRuler isNil])
			ifTrue: [ textMorph withLineNumbers ].
		(showLineNumbers not and: [ textMorph lineNumbersRuler isNotNil ])
			ifTrue: [ textMorph withoutLineNumbers ].
		inst updateLabel ]
]

{ #category : #controlling }
ClyTextLineNumbersSwitchMorph >> attachToTextMorph [
	super attachToTextMorph.

	label := StringMorph new.
	label setBalloonText:
			'Let you decide if the code pane should show the line numbers at the left of the code pane or not. +L: Click to add the lines number/L: Click to hide them.'.
	label on: #mouseDown send: #toggle to: self.
	self updateLabel.
	self addMorph: label.
	self class showLineNumbers
		ifTrue: [ textMorph withLineNumbers ]
]

{ #category : #operations }
ClyTextLineNumbersSwitchMorph >> toggle [
	textMorph lineNumbersRuler
		ifNil: [ textMorph withLineNumbers ]
		ifNotNil: [ textMorph withoutLineNumbers ].
	self updateLabel
]

{ #category : #updating }
ClyTextLineNumbersSwitchMorph >> updateLabel [

	| newValue |
	newValue := textMorph lineNumbersRuler ifNil: [ '+L' ] ifNotNil: [ 'L' ].
	label contents: newValue
]
