"
I test SHCustomStyle, the 'Custom' style editable in the settings.
"
Class {
	#name : 'SHCustomStyleTest',
	#superclass : 'TestCase',
	#instVars : [
		'savedStyles'
	],
	#category : 'Shout-Tests-Styling',
	#package : 'Shout-Tests',
	#tag : 'Styling'
}

{ #category : 'running' }
SHCustomStyleTest >> setUp [

	super setUp.
	savedStyles := Dictionary new.
	SHCustomStyle groups keysDo: [ :groupName |
		savedStyles at: groupName put: { 
			'color' -> (SHCustomStyle styleForGroup: groupName) color.
			'emphasis' -> (SHCustomStyle styleForGroup: groupName) emphasis } asDictionary ]
]

{ #category : 'running' }
SHCustomStyleTest >> tearDown [

	savedStyles keysAndValuesDo: [ :groupName :attributes | | style |
		style := SHStyleElement new
						color: (attributes at: 'color') ;
						emphasis: (attributes at: 'emphasis') ;
						yourself.
		SHCustomStyle style: style forGroup: groupName ].
	super tearDown
]

{ #category : 'tests' }
SHCustomStyleTest >> testAllStyleAccessors [
	"Test that all groups have corresponding accessors"
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
	"ensure that the color in the custom style is actually different"
	SHPreferences activeStyleTable = 'Custom' ifFalse: [ 
		(SHCustomStyle styleForGroup: 'default') 
			color: currentColor lighter negated ].

	SHCustomStyle copyFromCurrentStyle.

	self
		assert: (SHCustomStyle styleForGroup: 'default') color
		equals: currentColor
]
