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
	#category : #'Calypso-Browser-TextEditors'
}

{ #category : #controlling }
ClyTextLineNumbersSwitchMorph >> attachToTextMorph [ 
	super attachToTextMorph.
	
	label := StringMorph new.
	label setBalloonText:
			'Let you decide if the code pane should show the line numbers at the left of the code pane or not. +L: Click to add the lines number/L: Click to hide them.'.
	label on: #mouseDown send: #toggle to: self.
	self updateLabel.
	self addMorph: label
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
