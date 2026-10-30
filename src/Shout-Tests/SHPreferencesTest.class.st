"
I test SHPreferences, the settings of Shout.
"
Class {
	#name : 'SHPreferencesTest',
	#superclass : 'TestCase',
	#instVars : [
		'savedActiveStyleTable',
		'savedStyleTable'
	],
	#category : 'Shout-Tests-Styling',
	#package : 'Shout-Tests',
	#tag : 'Styling'
}

{ #category : 'running' }
SHPreferencesTest >> setUp [

	super setUp.
	savedActiveStyleTable := SHPreferences instVarNamed: #activeStyleTable.
	savedStyleTable := SHRBTextStyler styleTable
]

{ #category : 'running' }
SHPreferencesTest >> tearDown [

	SHPreferences instVarNamed: #activeStyleTable put: savedActiveStyleTable.
	SHRBTextStyler styleTable: savedStyleTable.
	super tearDown
]

{ #category : 'tests' }
SHPreferencesTest >> testThemeChangeKeepsAChosenStyle [

	SHPreferences activeStyleTable: 'Solarized'.

	SHPreferences themeChanged.

	self assert: SHRBTextStyler styleTable equals: SHRBTextStyler solarizedStyleTable
]

{ #category : 'tests' }
SHPreferencesTest >> testThemeChangeUpdatesTheStyleWhenNoStyleWasChosen [

	SHPreferences instVarNamed: #activeStyleTable put: nil.
	SHRBTextStyler styleTable: SHRBTextStyler solarizedStyleTable.

	SHPreferences themeChanged.

	self
		assert: SHRBTextStyler styleTable
		equals: (SHRBTextStyler perform: Smalltalk ui theme shStyleTableName)
]
