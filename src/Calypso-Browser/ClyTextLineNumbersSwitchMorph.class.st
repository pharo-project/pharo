"
I am a clickable label to toggle text morph decoration with line numbers.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	label:		<StringMorph>
"
Class {
	#name : 'ClyTextLineNumbersSwitchMorph',
	#superclass : 'ClyTextMorphToolMorph',
	#instVars : [
		'label'
	],
	#classInstVars : [
		'showLineNumbers'
	],
	#category : 'Calypso-Browser-TextEditors',
	#package : 'Calypso-Browser',
	#tag : 'TextEditors'
}

{ #category : 'settings' }
ClyTextLineNumbersSwitchMorph class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #showLineNumbers)
		label: 'Show lines numbers for all browsers';
		parent: #Calypso;
		default: false;
		target: self;
		description: 'If true, line numbers will appear in new system browsers for method editor'
]

{ #category : 'settings' }
ClyTextLineNumbersSwitchMorph class >> showLineNumbers [
	^ showLineNumbers ifNil: [ false ]
]

{ #category : 'settings' }
ClyTextLineNumbersSwitchMorph class >> showLineNumbers: aBoolean [

	showLineNumbers := aBoolean.
	self allInstancesDo: [ :inst |
			| textMorph |
			textMorph := inst textMorph.
			(showLineNumbers and: [ textMorph lineNumbersRuler isNil ]) ifTrue: [ textMorph withLineNumbers ].
			(showLineNumbers not and: [ textMorph lineNumbersRuler isNotNil ]) ifTrue: [ textMorph withoutLineNumbers ] ]
]

{ #category : 'controlling' }
ClyTextLineNumbersSwitchMorph >> attachToTextMorph [

	super attachToTextMorph.

	label := self theme
		         newCheckboxIn: self
		         for: self
		         getSelected: #isRuleActive  
		         setSelected: #toggle
		         getEnabled: #isEnabled
		         label: 'Line numbers'
		         help:
		         'Let you decide if the code pane should show the line numbers at the left of the code pane or not. Toggle the check box to add line numbers/hide them.'.

	label
		changeTableLayout;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap.
	label update: #isRuleActive.  
	self addMorph: label
]

{ #category : 'testing' }
ClyTextLineNumbersSwitchMorph >> isEnabled [

	self class showLineNumbers ifTrue: [ textMorph withLineNumbers ].

	^ self class showLineNumbers
]

{ #category : 'testing' }
ClyTextLineNumbersSwitchMorph >> isRuleActive [

	^ textMorph lineNumbersRuler notNil
]

{ #category : 'operations' }
ClyTextLineNumbersSwitchMorph >> toggle [

	textMorph lineNumbersRuler
		ifNil: [ textMorph withLineNumbers ]
		ifNotNil: [ textMorph withoutLineNumbers ] 
]
