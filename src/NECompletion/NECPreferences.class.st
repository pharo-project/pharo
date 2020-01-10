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
		'smartCharactersWithDoubleSpace'
	],
	#category : #'NECompletion-Model'
}

{ #category : #private }
NECPreferences class >> availableEngines [
	^ CompletionEngine withAllSubclasses
]

{ #category : #accessing }
NECPreferences class >> availablePopupShortcuts [
	^ {Character space shift . Character tab asKeyCombination}
]

{ #category : #private }
NECPreferences class >> availableSorters [
	^ CompletionSorter subclasses
]

{ #category : #accessing }
NECPreferences class >> backgroundColor [
	^ backgroundColor
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

{ #category : #defaults }
NECPreferences class >> defaultBackgroundColor [
	^ self theme menuColor
]

{ #category : #defaults }
NECPreferences class >> defaultPopupDelay [
	^ 200
]

{ #category : #defaults }
NECPreferences class >> defaultPopupShortcut [
	^ Character tab asShortcut
]

{ #category : #defaults }
NECPreferences class >> defaultPopupShowAutomatic [
	^ true
]

{ #category : #defaults }
NECPreferences class >> defaultSpaceAfterCompletion [
	^ true
]

{ #category : #defaults }
NECPreferences class >> defaultUseEnterToAccept [
	^ true
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
	caseSensitive := true.
	smartCharacters := true.
	expandPrefixes := true.
	captureNavigationKeys := true.
	useEnterToAccept := self defaultUseEnterToAccept.
	smartCharactersMapping := Dictionary new.
	smartCharactersMapping
		at: $( put: $) -> true;
		at: $[ put: $] -> true;
		at: ${ put: $} -> true;
		at: $" put: $" -> true;
		at: $' put: $' -> true;
		at: $< put: $> -> false;
		at: $| put: $| -> false.

	smartCharactersWithSingleSpace := ''.
	smartCharactersWithDoubleSpace := '[]{}'.
	backgroundColor := self defaultBackgroundColor.
	popupShowWithShortcut := self defaultPopupShortcut.
	popupShowAutomatic := self defaultPopupShowAutomatic.
	popupAutomaticDelay := self defaultPopupDelay.
	spaceAfterCompletion := true
]

{ #category : #accessing }
NECPreferences class >> popupAutomaticDelay [
	^ popupAutomaticDelay
]

{ #category : #accessing }
NECPreferences class >> popupAutomaticDelay: anObject [
	popupAutomaticDelay := anObject
]

{ #category : #accessing }
NECPreferences class >> popupShowAutomatic [
	^ popupShowAutomatic
]

{ #category : #accessing }
NECPreferences class >> popupShowAutomatic: aBoolean [
	popupShowAutomatic := aBoolean
]

{ #category : #accessing }
NECPreferences class >> popupShowWithShortcut [
	^ popupShowWithShortcut
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
		default: true;
		iconName: #smallConfigurationIcon;
		description: 'Enable or disable code completion in browsers, debuggers and workspaces.';
		with: [ 
					| availableControllers availableSorters |
					availableControllers := self availableEngines.
					availableControllers size > 1
						ifTrue: [ 
							(aBuilder pickOne: #completionEngineClass)
								order: -1;
								label: 'CompletionEngine';
								target: RubSmalltalkEditor;
								getSelector: #completionEngineClass;
								setSelector: #completionEngineClass:;
								domainValues: availableControllers ].
					availableSorters := self availableSorters.
					availableSorters size > 1
						ifTrue: [ 
							(aBuilder pickOne: #completionSorter)
								order: -1;
								label: 'Sorter';
								target: CompletionContext;
								getSelector: #sorterClass;
								setSelector: #sorterClass:;
								domainValues: availableSorters ].
					(aBuilder setting: #backgroundColor)
						default: Color veryDarkGray;
						label: 'Background Color'.
					(aBuilder setting: #expandPrefixes)
						default: true;
						label: 'Complete common prefixes using TAB in the suggested completions'.
					(aBuilder setting: #useEnterToAccept)
						label: 'Use ENTER to accept a suggested completion';
						default: self defaultUseEnterToAccept.
					(aBuilder setting: #captureNavigationKeys)
						default: true;
						label: 'Use navigation keys for extended completion functionality'.
					(aBuilder setting: #caseSensitive)
						label: 'Case Sensitive';
						default: true;
						description: 'Decide if you want eCompletion to be case sensitive or not.'.
					(aBuilder setting: #smartCharacters)
						label: 'Smart Characters';
						default: true;
						description: 'Decide if you want eCompletion to use smart characters, e.g, to automatically close brackets.'.
					(aBuilder setting: #smartCharactersWithSingleSpace)
						label: 'Smart Characters with Single Space';
						default: '';
				 		description: 'Enumerate the characters which are automatically inserted with a single space in between.'.
					(aBuilder setting: #smartCharactersWithDoubleSpace) 
						label: 'Smart Characters with Double Space';
						default: '[]{}';
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
	^ spaceAfterCompletion
]

{ #category : #accessing }
NECPreferences class >> spaceAfterCompletion: anObject [
	spaceAfterCompletion := anObject
]

{ #category : #accessing }
NECPreferences class >> theme [
	self flag: #SmalltalkReference.
	^ Smalltalk ui theme
]

{ #category : #accessing }
NECPreferences class >> useEnterToAccept [
	^ useEnterToAccept
]

{ #category : #accessing }
NECPreferences class >> useEnterToAccept: aBoolean [
	useEnterToAccept := aBoolean
]
