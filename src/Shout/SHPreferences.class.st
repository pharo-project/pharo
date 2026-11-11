"
I manage the settings for Shout.

This includes:
  - Turning syntax highlighting on / off
  - Choosing a predefined style
  - Editing the custom style table
"
Class {
	#name : 'SHPreferences',
	#superclass : 'Object',
	#classInstVars : [
		'activeStyleTable',
		'syntaxHighlightingEnabled'
	],
	#category : 'Shout-Styling',
	#package : 'Shout',
	#tag : 'Styling'
}

{ #category : 'accessing' }
SHPreferences class >> activeStyleTable [

	^ activeStyleTable ifNil: [
		self activeStyleTable: 'Default'.
		activeStyleTable ]
]

{ #category : 'accessing' }
SHPreferences class >> activeStyleTable: aStyleTableName [

	(self styleTableNames includes: aStyleTableName) ifFalse: [ 
		self error: 'Trying to activate unknown style table' ].
	(Pragma allNamed: #styleTable: in: SHRBTextStyler class)
		detect: [ :pragma | (pragma argumentAt: 1) = aStyleTableName ]
		ifFound: [ :pragma |
			self styleTable: (SHRBTextStyler styleTableNamed: aStyleTableName) ]
		ifNone: [ "ignore" ].
	'Custom' = aStyleTableName ifTrue: [
		self styleTable: SHCustomStyle styleTable ].
	'Default' = aStyleTableName ifTrue: [
			self styleTable:
				(SHRBTextStyler perform: Smalltalk ui theme shStyleTableName) ].
	activeStyleTable := aStyleTableName
]

{ #category : 'updating' }
SHPreferences class >> customStyleChanged [

	self activeStyleTable = 'Custom' ifTrue: [
		self styleTable: SHCustomStyle styleTable ]
]

{ #category : 'accessing' }
SHPreferences class >> enabled [

	^ syntaxHighlightingEnabled ifNil: [ ^ true ]
]

{ #category : 'accessing' }
SHPreferences class >> enabled: aBoolean [

	syntaxHighlightingEnabled := aBoolean.
	self informUI
]

{ #category : 'private' }
SHPreferences class >> informUI [
	"Update all editor panes using syntax highlighting"
	self currentWorld themeChanged
]

{ #category : 'class initialization' }
SHPreferences class >> initialize [

	SHCustomStyle when: #changed send: #customStyleChanged to: self.
	
]

{ #category : 'settings' }
SHPreferences class >> settingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: 'Syntax Highlighting')
		parentName: 'codeBrowsing';
		description:
			'Syntax Highlighting As You Type: Enable syntax highlighting in browsers, debuggers and workspaces and set patterns style.';
		isActive: true;
		readBlock: [ self enabled ];
		writeBlock: [ :active | self enabled: active ];
		target: self;
		iconName: #smallConfiguration;
		noOrdering;
		with: [
				(aBuilder setting: #formatIncompleteIdentifiers)
					target: SHRBTextStyler;
					defaultValue: false;
					label: 'Format Incomplete Identifiers';
					parentName: 'Syntax Highlighting';
					description:
						'If the code highlighter tryies to format incomplete identifiers and selectors or not. This is not recommended for big images, as it traverse all the image to get the information'.
				(aBuilder pickOne: #activeStyleTable)
					label: 'Predefined styles';
					target: self;
					domainValues: self styleTableNames;
					description:
						'Choose between the different styles.  Default changes according to the UI Theme and Custom can be modified by you.';
					default: 'Default'.]
]

{ #category : 'private' }
SHPreferences class >> styleTable: anArray [

	SHRBTextStyler styleTable: anArray.
	self informUI
]

{ #category : 'accessing' }
SHPreferences class >> styleTableNames [

	^ { 'Custom'. 'Default' }
	  , ((Pragma allNamed: #styleTable: in: SHRBTextStyler class) collect: [:pragma | pragma argumentAt: 1])
]

{ #category : 'updating' }
SHPreferences class >> themeChanged [

	(activeStyleTable isNil or: [ activeStyleTable = 'Default' ])
		ifTrue: [ self activeStyleTable: 'Default' ]
]
