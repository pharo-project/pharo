"
I am a clickable label to toggle wrap mode of text morph.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	wrapModeLabel:		<StringMorph>
"
Class {
	#name : 'ClyTextWrapModeSwitchMorph',
	#superclass : 'ClyTextMorphToolMorph',
	#instVars : [
		'wrapModeCheckBox'
	],
	#category : 'Calypso-Browser-TextEditors',
	#package : 'Calypso-Browser',
	#tag : 'TextEditors'
}

{ #category : 'controlling' }
ClyTextWrapModeSwitchMorph >> attachToTextMorph [

	super attachToTextMorph.

	wrapModeCheckBox := self theme
		                    newCheckboxIn: self
		                    for: self
		                    getSelected: #isWrapped
		                    setSelected: #toggle
		                    getEnabled: nil
		                    label: 'Word Wrap'
		                    help:
		                    'Let you decide if the code pane should automaticaly wrap lines if a line reach the end of the line. W: Wrap / NW: Add a scrollbar.'.

	wrapModeCheckBox
		changeTableLayout;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap.
	self addMorph: wrapModeCheckBox
]

{ #category : 'testing' }
ClyTextWrapModeSwitchMorph >> isWrapped [

	^ textMorph wrapped
]

{ #category : 'operations' }
ClyTextWrapModeSwitchMorph >> toggle [

	self isWrapped
		ifTrue: [ textMorph beNotWrapped ]
		ifFalse: [ textMorph beWrapped ]
]
