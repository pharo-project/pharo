"
I control the preferences for completion framework. 
It is the same for NECompletion and NOCompletion (and other potential completion algorithms should be configured here too)


"
Class {
	#name : #NECPreferences,
	#superclass : #Object,
	#classInstVars : [
		'enabled',
		'caseSensitive',
		'smartCharacters',
		'smartCharactersMapping',
		'backgroundColor',
		'expandPrefixes',
		'captureNavigationKeys',
		'useEnterToAccept',
		'popupShowAutomatic',
		'popupAutomaticDelay',
		'popupShowWithShortcut',
		'spaceAfterCompletion',
		'smartCharactersWithSingleSpace',
		'smartCharactersWithDoubleSpace',
		'overrideModel'
	],
	#category : #'NECompletion-Model'
}

{ #category : #private }
NECPreferences class >> availableControllers [
	^NECController withAllSubclasses copyWithout: NECWorkspaceController
]

{ #category : #accessing }
NECPreferences class >> availablePopupShortcuts [
	^ {
		Character space shift. 
		Character tab asKeyCombination}
]

{ #category : #accessing }
NECPreferences class >> backgroundColor [ 
	^backgroundColor ifNil: [ 
		backgroundColor := (Smalltalk ui theme settings menuColor ifNil: [ Color white ]) ]
]

{ #category : #accessing }
NECPreferences class >> backgroundColor: aColor [ 
	backgroundColor := aColor
]

{ #category : #accessing }
NECPreferences class >> captureNavigationKeys [
	^ captureNavigationKeys
]

{ #category : #accessing }
NECPreferences class >> captureNavigationKeys: aBoolean [
	captureNavigationKeys := aBoolean
]

{ #category : #accessing }
NECPreferences class >> caseSensitive [
	^ caseSensitive

]

{ #category : #accessing }
NECPreferences class >> caseSensitive: aBoolean [
	caseSensitive := aBoolean
]

{ #category : #private }
NECPreferences class >> currentController [ 
	^ (Smalltalk tools hasToolNamed: #codeCompletion)
		ifTrue: [ Smalltalk tools codeCompletion ]
		ifFalse: [ nil ]
]

{ #category : #defaults }
NECPreferences class >> defaultPopupDelay [
	^ 200
]

{ #category : #defaults }
NECPreferences class >> defaultPopupShortcut [
	^Character tab asShortcut
]

{ #category : #defaults }
NECPreferences class >> defaultPopupShowAutomatic [
	^true
]

{ #category : #defaults }
NECPreferences class >> defaultSpaceAfterCompletion [
	^true
]

{ #category : #defaults }
NECPreferences class >> defaultUseEnterToAccept [
	
	^ false
]

{ #category : #accessing }
NECPreferences class >> enabled [
	^ enabled

]

{ #category : #accessing }
NECPreferences class >> enabled: aBoolean [
	enabled := aBoolean.
]

{ #category : #accessing }
NECPreferences class >> expandPrefixes [
	^ expandPrefixes
]

{ #category : #accessing }
NECPreferences class >> expandPrefixes: aBoolean [
	expandPrefixes := aBoolean
]

{ #category : #'class initialization' }
NECPreferences class >> initialize [
	enabled := true.
	overrideModel := false.
	caseSensitive := true.
	smartCharacters := true.
	expandPrefixes := true.
	captureNavigationKeys := false.
	useEnterToAccept := true.
	smartCharactersMapping := Dictionary new.
	smartCharactersMapping
		at: $( put: $);
		at: $[ put: $];
		at: ${ put: $};
		at: $" put: $";
		at: $' put: $';
		at: $< put: $>;
		at: $| put: $|.
		
	smartCharactersWithSingleSpace := ''.
	smartCharactersWithDoubleSpace := '[]{}'.
	backgroundColor := Smalltalk ui theme settings menuColor ifNil: [ Color white ].
	popupShowWithShortcut := self defaultPopupShortcut.
	popupShowAutomatic := self defaultPopupShowAutomatic.
	popupAutomaticDelay := self defaultPopupDelay.
	spaceAfterCompletion := true
]

{ #category : #accessing }
NECPreferences class >> overrideModel [ 
	^overrideModel ifNil: [ overrideModel := false].
]

{ #category : #accessing }
NECPreferences class >> overrideModel: aBoolean [
	overrideModel := aBoolean
]

{ #category : #accessing }
NECPreferences class >> popupAutomaticDelay [
	^popupAutomaticDelay ifNil: [ popupAutomaticDelay := self defaultPopupDelay ] 
]

{ #category : #accessing }
NECPreferences class >> popupAutomaticDelay: anObject [
	popupAutomaticDelay := anObject
]

{ #category : #accessing }
NECPreferences class >> popupShowAutomatic [
	^ popupShowAutomatic ifNil: [ popupShowAutomatic := self defaultPopupShowAutomatic ]
]

{ #category : #accessing }
NECPreferences class >> popupShowAutomatic: aBoolean [
	popupShowAutomatic := aBoolean
]

{ #category : #accessing }
NECPreferences class >> popupShowWithShortcut [
	^ popupShowWithShortcut ifNil: [ popupShowWithShortcut := self defaultPopupShortcut ]
]

{ #category : #accessing }
NECPreferences class >> popupShowWithShortcut: anObject [
	popupShowWithShortcut := anObject
]

{ #category : #settings }
NECPreferences class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #'Code Completion')
		target: self;
		parentName: #codeBrowsing;
		selector: #enabled;
		iconName: #smallConfigurationIcon;
		description: 'Enable or disable code completion in browsers, debuggers and workspaces.';
		with: [ 
					| availableControllers |
					availableControllers := self availableControllers.
					availableControllers size > 1
						ifTrue: [ 
							(aBuilder pickOne: #completionController)
								order: -1;
								label: 'Controller';
								getSelector: #currentController;
								setSelector: #useController:;
								domainValues: availableControllers ].
					(aBuilder setting: #backgroundColor)
						label: 'Background Color'.
					(aBuilder setting: #overrideModel)
						label: 'Complete whole methods'.	
					(aBuilder setting: #expandPrefixes)
						label: 'Complete common prefixes using TAB in the suggested completions'.
					(aBuilder setting: #useEnterToAccept)
						label: 'Use ENTER to accept a suggested completion';
						default: self defaultUseEnterToAccept.
					(aBuilder setting: #captureNavigationKeys)
						label: 'Use navigation keys for extended completion functionality'.
					(aBuilder setting: #caseSensitive)
						label: 'Case Sensitive';
						description: 'Decide if you want eCompletion to be case sensitive or not.'.
					(aBuilder setting: #smartCharacters)
						label: 'Smart Characters';
						description: 'Decide if you want eCompletion to use smart characters, e.g, to automatically close brackets.'.
					(aBuilder setting: #smartCharactersWithSingleSpace)
						label: 'Smart Characters with Single Space';
				 		description: 'Enumerate the characters which are automatically inserted with a single space in between.'.
					(aBuilder setting: #smartCharactersWithDoubleSpace) 
						label: 'Smart Characters with Double Space';
				 		description: 'Enumerate the characters which are automatically inserted with a two spaces in between.'.
					(aBuilder setting: #popupShowAutomatic)
						default: self defaultPopupShowAutomatic;
						label: 'Popup is automatic'.
					(aBuilder setting: #popupAutomaticDelay)
						default: self defaultPopupDelay;
						label: 'Popup appearance delay'.
					(aBuilder pickOne: #popupShowWithShortcut)
						default: self defaultPopupShortcut;
						label: 'Popup appears with this shortcut';
						domainValues: self availablePopupShortcuts.
					(aBuilder setting: #spaceAfterCompletion)
						default: self defaultSpaceAfterCompletion;
						label: 'Put a space after completion'
					 ].
]

{ #category : #accessing }
NECPreferences class >> smartCharacters [
	^ smartCharacters
]

{ #category : #accessing }
NECPreferences class >> smartCharacters: aBoolean [
	smartCharacters := aBoolean
]

{ #category : #accessing }
NECPreferences class >> smartCharactersMapping [
	^ smartCharactersMapping
]

{ #category : #accessing }
NECPreferences class >> smartCharactersMapping: aDictionary [
	smartCharactersMapping := aDictionary
]

{ #category : #accessing }
NECPreferences class >> smartCharactersWithDoubleSpace [
	^ smartCharactersWithDoubleSpace
]

{ #category : #accessing }
NECPreferences class >> smartCharactersWithDoubleSpace: aString [
	smartCharactersWithDoubleSpace := aString
]

{ #category : #accessing }
NECPreferences class >> smartCharactersWithSingleSpace [
	^ smartCharactersWithSingleSpace
]

{ #category : #accessing }
NECPreferences class >> smartCharactersWithSingleSpace: aString [
	smartCharactersWithSingleSpace := aString
]

{ #category : #accessing }
NECPreferences class >> spaceAfterCompletion [
	^ spaceAfterCompletion ifNil: [ spaceAfterCompletion := true ]
]

{ #category : #accessing }
NECPreferences class >> spaceAfterCompletion: anObject [
	spaceAfterCompletion := anObject
]

{ #category : #private }
NECPreferences class >> useController: aClass [
	aClass registerToolsOn: Smalltalk tools
]

{ #category : #accessing }
NECPreferences class >> useEnterToAccept [
	^ useEnterToAccept ifNil: [ useEnterToAccept := self defaultUseEnterToAccept ]
]

{ #category : #accessing }
NECPreferences class >> useEnterToAccept: aBoolean [
	useEnterToAccept := aBoolean
]
