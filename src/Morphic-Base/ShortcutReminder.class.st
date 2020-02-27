"
ShortcutReminder is an object used to bring up a notification whenever a menu entry that has a keyboard shortcut is clicked on instead of using the keyboard.

It is configurable from the settings browser, where one can choose the location and size of the notification Morph. The notification also holds a count of how many times a specific menu entry has been reminded for, and will only notify up to that limit. It is possible to set this limit to -1 such that it will always notify.

"
Class {
	#name : #ShortcutReminder,
	#superclass : #Object,
	#instVars : [
		'limit',
		'size',
		'position',
		'countDict',
		'backgroundColor',
		'textColor'
	],
	#classVars : [
		'DefaultReminder',
		'Enabled'
	],
	#category : #'Morphic-Base-Menus'
}

{ #category : #defaults }
ShortcutReminder class >> defaultReminder [

	^DefaultReminder ifNil: [ DefaultReminder := self new ]
]

{ #category : #settings }
ShortcutReminder class >> enabled [

	^Enabled ifNil: [ Enabled := false ]
]

{ #category : #settings }
ShortcutReminder class >> enabled: aBoolean [

	Enabled := aBoolean
]

{ #category : #settings }
ShortcutReminder class >> settingsOn: aBuilder [ 
	<systemsettings>
	(aBuilder setting: #'Shortcut Reminder') 
		target: self;
		parent: #appearance;
		default: true;
		description: 'Shortcut reminder is a reminding tool which pops up when a menu entry which includes a keyboard shortcut is invoked by the mouse instead';
		selector: #enabled;
		iconName: #smallConfigurationIcon;
		with: [ self defaultReminder customSettingsOn: aBuilder ]
]

{ #category : #display }
ShortcutReminder class >> showShortcut: aShortcut [
	
	(aShortcut keyText isNil or: [ self enabled not ]) ifTrue: [ ^self ].
	
	self defaultReminder remind: aShortcut.
]

{ #category : #accessing }
ShortcutReminder >> backgroundColor [

	^backgroundColor 
]

{ #category : #accessing }
ShortcutReminder >> backgroundColor: aColor [

	backgroundColor := aColor
]

{ #category : #'private - utilities' }
ShortcutReminder >> createKeyTextMorph: aString [

	^(self fixKeyText: aString) asStringMorph 
		font: self keyTextFont emphasis: 2;
		color: textColor;
		yourself. 
]

{ #category : #'private - utilities' }
ShortcutReminder >> createLabelMorph: aString [

	^aString asStringMorph 
		font: self labelFont emphasis: 0;
		color: textColor;
		yourself. 
]

{ #category : #settings }
ShortcutReminder >> createResetCountButtonMorph [

	^SimpleButtonMorph new
		target: self;
		label: 'Reset notification counts';
		actionSelector: #resetCount;
		themeChanged;
		yourself
	
]

{ #category : #'shortcut creation' }
ShortcutReminder >> createShortcutMorphFor: aMenuItem [ 

	^FadingMorph new
		addMorphBack: (self createLabelMorph: aMenuItem contents);
		addMorphBack: (self createKeyTextMorph: aMenuItem keyText);
		color: backgroundColor;
		yourself

]

{ #category : #settings }
ShortcutReminder >> customSettingsOn: aBuilder [

	(aBuilder pickOne: #position)
		order: 1;
		target: self;
		label: 'Position in World';
		description: 'Select one of the positons available to specify where the shortcut notification morph will be shown';
		default: #bottomLeft;
		domainValues: self positions.
		
	(aBuilder pickOne: #textSize) 
		order: 2;
		target: self;
		label: 'Size of the reminder';
		description: 'Set the size (small medium or large) of the reminder morph shown';
		domainValues: self sizes.
		
	(aBuilder setting: #textColor)
		order: 3;
		target: self;
		label: 'Text color';
		default: self defaultTextColor.
		
	(aBuilder setting: #backgroundColor)
		target: self;
		label: 'Background color';
		default: self defaultBackgroundColor.
		
	(aBuilder range: #limit) 
		order: 6;
		target: self; 
      label: 'Notification limit' ;
		description: 'Set the limit of how many notifications per menu entry one should be reminded about. Setting the value to -1 means that it will allways remind you' ;
		default: self defaultLimit;
		range: (-1 to: 100).
		
	(aBuilder group: #'Reset count')
		order: 7;
		target: self;
		label: 'Reset the reminder count';
		dialog: [ self createResetCountButtonMorph ].
]

{ #category : #defaults }
ShortcutReminder >> defaultBackgroundColor [

	^Color gray
]

{ #category : #defaults }
ShortcutReminder >> defaultFontFamiliyName [

	^'Source Sans Pro'
]

{ #category : #defaults }
ShortcutReminder >> defaultLimit [

	^5
]

{ #category : #defaults }
ShortcutReminder >> defaultTextColor [

	^Color black
]

{ #category : #'private - utilities' }
ShortcutReminder >> fixKeyText: aString [

	^ToggleMenuItemShortcut normalize: aString
]

{ #category : #'private - utilities' }
ShortcutReminder >> fontSized: aPointSize [

	^LogicalFont familyName: self defaultFontFamiliyName pointSize: aPointSize
]

{ #category : #initialization }
ShortcutReminder >> initialize [

	countDict := Dictionary new.
	limit := self defaultLimit.
	size := #small.
	position := #bottomLeft.
	backgroundColor := self defaultBackgroundColor.
	textColor := self defaultTextColor.

]

{ #category : #private }
ShortcutReminder >> keyTextFont [

	^self fontSized: self pointSize * 2
]

{ #category : #private }
ShortcutReminder >> labelFont [

	^self fontSized: self pointSize 
]

{ #category : #private }
ShortcutReminder >> large [

	^55
]

{ #category : #accessing }
ShortcutReminder >> limit [
	
	^limit
]

{ #category : #accessing }
ShortcutReminder >> limit: aNumber [

	limit := aNumber
]

{ #category : #private }
ShortcutReminder >> medium [

	 ^27
]

{ #category : #private }
ShortcutReminder >> pointSize [

	^size value: self
]

{ #category : #accessing }
ShortcutReminder >> position [

	^position
]

{ #category : #accessing }
ShortcutReminder >> position: aSymbol [

	(self positions includes: aSymbol) ifFalse: [ self error ].
	
	position := aSymbol
]

{ #category : #'shortcut creation' }
ShortcutReminder >> positionShortcut: aMorph [
	
	| area |
	
	area := self currentWorld clearArea insetBy: 3. 
	
	aMorph 
	cellPositioning: position;
	fullBounds. 
	"the layout has to be computed before changing the position."
	
	aMorph perform: (position , ':') asSymbol with: (position value: area).
	
]

{ #category : #private }
ShortcutReminder >> positions [

	^#(#topLeft #topRight #center #bottomLeft #bottomRight)
]

{ #category : #'shortcut creation' }
ShortcutReminder >> remind: aMenuItem [ 

	| itemHash |
	
	itemHash := (aMenuItem contents hash + aMenuItem keyText hash) hashMultiply.
	
	(self wantsToShow: itemHash) ifFalse: [ ^self ].
	
	self 
	show: aMenuItem;
	updateCount: itemHash.
]

{ #category : #settings }
ShortcutReminder >> resetCount [

	countDict := Dictionary new. 
	
]

{ #category : #'shortcut creation' }
ShortcutReminder >> show: aMenuItem [

	| shortcutMorph |
	
	shortcutMorph := self createShortcutMorphFor: aMenuItem.
	
	self positionShortcut: shortcutMorph.
	
	shortcutMorph openInWorld.
]

{ #category : #private }
ShortcutReminder >> sizes [ 

	^#(#small #medium #large) 
]

{ #category : #private }
ShortcutReminder >> small [

	^15
]

{ #category : #accessing }
ShortcutReminder >> textColor [

	^textColor
]

{ #category : #accessing }
ShortcutReminder >> textColor: aColor [

	textColor := aColor
]

{ #category : #accessing }
ShortcutReminder >> textSize [

	^size
]

{ #category : #accessing }
ShortcutReminder >> textSize: aSymbol [

	(self sizes includes: aSymbol) ifFalse: [ self error ].
	
	size := aSymbol
]

{ #category : #private }
ShortcutReminder >> updateCount: aHashValue [
	
	| value |
	
	value := countDict at: aHashValue ifAbsent: [ 0 ].
	
	countDict at: aHashValue put: value + 1.
]

{ #category : #queries }
ShortcutReminder >> wantsToShow: aHashValue [
	
	^limit < 0 or: [ 
		(countDict at: aHashValue ifAbsent: [ 0 ]) < limit
	]
]
