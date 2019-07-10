"
I am a clickable label to toggle wrap mode of text morph.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	wrapModeLabel:		<StringMorph>
"
Class {
	#name : #ClyTextWrapModeSwitchMorph,
	#superclass : #ClyTextMorphToolMorph,
	#instVars : [
		'wrapModeLabel'
	],
	#category : #'Calypso-Browser-TextEditors'
}

{ #category : #controlling }
ClyTextWrapModeSwitchMorph >> attachToTextMorph [ 
	super attachToTextMorph.
	
	wrapModeLabel := StringMorph new.
	wrapModeLabel setBalloonText:
			'Let you decide if the code pane should automaticaly wrap lines if a line reach the end of the line. W: Wrap / NW: Add a scrollbar.'.
	wrapModeLabel on: #mouseDown send: #toggle to: self.
	self updateLabel.
	self addMorph: wrapModeLabel
]

{ #category : #operations }
ClyTextWrapModeSwitchMorph >> toggle [
	textMorph wrapped 
		ifTrue: [ textMorph beNotWrapped  ]
		ifFalse: [ textMorph beWrapped ].
	self updateLabel
]

{ #category : #updating }
ClyTextWrapModeSwitchMorph >> updateLabel [

	| newValue |
	newValue := textMorph wrapped ifTrue: [ 'W' ] ifFalse: [ 'NW' ].
	wrapModeLabel contents: newValue
]
