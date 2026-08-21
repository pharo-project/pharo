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
		'enabled',
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
			  activeStyleTable := 'Default'.
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

	activeStyleTable = 'Custom' ifTrue: [
		self styleTable: SHCustomStyle styleTable ]
]

{ #category : 'reflective operations' }
SHPreferences class >> doesNotUnderstand: aMessage [
	"Handles messages of the form #defaultStyle or #argumentStyle: "

	(aMessage selector endsWith: 'Style') ifTrue: [
			^ SHStyleElement group:
				  (aMessage selector withoutSuffix: 'Style') asSymbol ].
	(aMessage selector endsWith: 'Style:') ifTrue: [
		| style |
		style := aMessage argument.
		(SHStyleElement group: (style at: 'group'))
			color: (style at: 'color') ;
			emphasis: (style at: 'emphasis').
		^ self ].
	^ super doesNotUnderstand: aMessage
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

{ #category : 'settings' }
SHPreferences class >> settingsOn: aBuilder [
	
	<systemsettings>
	(aBuilder group: 'Syntax Highlighting')
		parentName: 'codeBrowsing';
		target: self;
		iconName: #smallConfiguration;
		noOrdering;
		with: [
				(aBuilder setting: 'Enable Syntax Highlighting')
					target: self;
					default: true;
					selector: #enabled;
					description:
						'Syntax Highlighting As You Type: Enable syntax highlighting in browsers, debuggers and workspaces and set patterns style.'.
				(aBuilder setting: #formatIncompleteIdentifiers)
					target: SHRBTextStyler;
					default: false;
					label: 'Format Incomplete Identifiers';
					parentName: 'Syntax Highlighting';
					description:
						'If the code highlighter tryies to format incomplete identifiers and selectors or not. This is not recommended for big images, as it traverse all the image to get the information'.
				(aBuilder pickOne: #activeStyleTable)
					label: 'Predefined styles';
					target: self;
					domainValues: self styleTableNames;
					description: 'Choose between the different styles.  Default changes according to the UI Theme and Custom can be modified by you.' ;
					default: 'Default'.
				(aBuilder group: 'Edit Custom Style')
					description: 'Change the Custom style to your liking.  You have to activate it for your changes to take effect.' ;
					dialog: [
							Smalltalk ui theme
								newButtonIn: self
								for: SHCustomStyle
								getState: nil
								action: #copyFromCurrentStyle
								arguments: nil
								getEnabled: nil
								label: 'Copy from current style'
								help:
									'Initialize custom style to colors of the currently selected them' ];
					with: [
							SHCustomStyle groups keysAndValuesDo: [ :group :info |
										(aBuilder setting: group , 'Style')
											target: self;
											label: (info at: 'label');
											description: (info at: 'description' ifAbsent: [ '' ]) ] ] ]
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

	activeStyleTable = 'Default' ifTrue: [ self activeStyleTable: 'Default' ]
]
