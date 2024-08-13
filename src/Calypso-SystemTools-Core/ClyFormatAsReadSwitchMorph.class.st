"
I am a tool to force method editor to automatically format given source code.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	checkbox:		<Object>
"
Class {
	#name : 'ClyFormatAsReadSwitchMorph',
	#superclass : 'ClyTextMorphToolMorph',
	#instVars : [
		'checkbox'
	],
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'formatting' }
ClyFormatAsReadSwitchMorph >> formatAsYouRead [

	^ ASTProgramNode formatterClass formatAsYouReadPolicy
]

{ #category : 'formatting' }
ClyFormatAsReadSwitchMorph >> formatAsYouReadLabel [
	^ 'F'
]

{ #category : 'initialization' }
ClyFormatAsReadSwitchMorph >> initialize [
	super initialize.

	checkbox := CheckboxMorph
		on: self selected: #formatAsYouRead changeSelected: #toggle.
	checkbox
		getLabelSelector: #formatAsYouReadLabel;
		labelClickable: true;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		setBalloonText:
			'Autoformat code without editing. Useful when reading some old/unformatted code. You can configure your own format in the settings browser.'.
	self addMorph: checkbox
]

{ #category : 'operations' }
ClyFormatAsReadSwitchMorph >> toggle [
	| formatter isDirty |
	formatter := ASTProgramNode formatterClass.
	formatter formatAsYouReadPolicy
		ifTrue: [
			formatter formatAsYouReadPolicy: false.
			textMorph model canDiscardEdits
				ifTrue: [ textMorph model cancelEdits ] ]
		ifFalse: [
			formatter formatAsYouReadPolicy: true.
			isDirty := textMorph hasUnacceptedEdits.
			textMorph model	formatSourceCodeInView.
			isDirty ifFalse: [ textMorph model clearUserEdits ]]
]
