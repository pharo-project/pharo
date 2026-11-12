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
	SHCustomStyle triggerEvent: #changed.
	super tearDown
]

{ #category : 'tests' }
SHCustomStyleTest >> testAllStyleAccessors [

	SHCustomStyle groups keysDo: [ :groupName | 
		| getter setter style color emphasis|
		getter := (groupName , 'Style') asSymbol.
		setter := (groupName , 'Style:') asSymbol. 
		style := SHCustomStyle perform: getter.
		color := style color.
		color := color ifNil: [ Color veryLightGray ] ifNotNil: [ color paler negated ].
		emphasis := style emphasis = #normal ifTrue: [ #bold ] ifFalse: [ #normal ].
		style := SHStyleElement new
			color: color  ;
			emphasis: emphasis ;
			yourself.
		SHCustomStyle 
			perform: setter 
			with: style.
		style := SHCustomStyle perform: getter.
		self assert: style color equals: color.
		self assert: style emphasis equals: emphasis.]
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

	| currentDefaultRow currentColor |
	currentDefaultRow := SHRBTextStyler styleTable detect: [ :row | row first = 'default' ].
	currentColor := (Color colorFrom: currentDefaultRow second).
	(SHCustomStyle styleForGroup: 'default') color: currentColor lighter negated.

	SHCustomStyle copyFromCurrentStyle.

	self
		assert: (SHCustomStyle styleForGroup: 'default') color
		equals: currentColor
]
