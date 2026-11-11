"
I test SHCustomStyle, the 'Custom' style editable in the settings.
"
Class {
	#name : 'SHCustomStyleTest',
	#superclass : 'TestCase',
	#instVars : [
		'savedGroups'
	],
	#category : 'Shout-Tests-Styling',
	#package : 'Shout-Tests',
	#tag : 'Styling'
}

{ #category : 'running' }
SHCustomStyleTest >> setUp [

	super setUp.
	savedGroups := SHCustomStyle groups deepCopy
]

{ #category : 'running' }
SHCustomStyleTest >> tearDown [

	SHCustomStyle instVarNamed: #groups put: savedGroups.
	super tearDown
]

{ #category : 'tests' }
SHCustomStyleTest >> testCopyFromCurrentStyleButtonIsASetting [

	| button |
	button := SettingsManager forSystemSettings settings detect: [ :each | each name = #copyFromCurrentStyle ].

	self assert: button class equals: ActionSetting.
	self assert: button target equals: SHCustomStyle.
	self assert: button parent name equals: #'Edit Custom Style'
]

{ #category : 'tests' }
SHCustomStyleTest >> testCopyFromCurrentStyleCopiesCurrentColors [

	| currentDefaultRow |
	currentDefaultRow := SHRBTextStyler styleTable detect: [ :row | row first = 'default' ].
	SHCustomStyle color: Color transparent forGroup: 'default'.

	SHCustomStyle copyFromCurrentStyle.

	self
		assert: (SHCustomStyle colorForGroup: 'default')
		equals: (Color colorFrom: currentDefaultRow second)
]
