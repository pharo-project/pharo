"
I control the preferences for completion framework. 
It is the same for NECompletion and NOCompletion (and other potential completion algorithms should be configured here too)


"
Class {
	#name : 'NECPreferences',
	#superclass : 'Object',
	#classInstVars : [
		'enabled',
		'caseSensitive',
		'smartCharacters',
		'smartCharactersMapping',
		'backgroundColor',
		'expandPrefixes',
		'useEnterToAccept',
		'popupShowAutomatic',
		'popupAutomaticDelay',
		'popupShowWithShortcut',
		'smartCharactersWithSingleSpace',
		'smartCharactersWithDoubleSpace',
		'showCompletionDetails'
	],
	#category : 'NECompletion-Preferences',
	#package : 'NECompletion-Preferences'
}

{ #category : 'private' }
NECPreferences class >> availableEngines [
	^ CompletionEngine allSubclasses
]

{ #category : 'accessing' }
NECPreferences class >> availablePopupShortcuts [
	^ {Character space shift . Character tab asKeyCombination}
]

{ #category : 'private' }
NECPreferences class >> availableSorters [
	^ CompletionSorter subclasses
]

{ #category : 'accessing' }
NECPreferences class >> backgroundColor [
	^ backgroundColor
]

{ #category : 'accessing' }
NECPreferences class >> backgroundColor: aColor [
	backgroundColor := aColor
]

{ #category : 'accessing' }
NECPreferences class >> caseSensitive [
	^ caseSensitive
]

{ #category : 'accessing' }
NECPreferences class >> caseSensitive: aBoolean [
	caseSensitive := aBoolean
]

{ #category : 'defaults' }
NECPreferences class >> defaultBackgroundColor [
	^ self theme menuColor
]

{ #category : 'defaults' }
NECPreferences class >> defaultPopupDelay [
	^ 200
]

{ #category : 'defaults' }
NECPreferences class >> defaultPopupShortcut [
	^ Character tab asShortcut
]

{ #category : 'defaults' }
NECPreferences class >> defaultPopupShowAutomatic [
	^ true
]

{ #category : 'defaults' }
NECPreferences class >> defaultShowCompletionDetails [

	^ true
]

{ #category : 'defaults' }
NECPreferences class >> defaultSpaceAfterCompletion [
	^ true
]

{ #category : 'defaults' }
NECPreferences class >> defaultUseEnterToAccept [
	^ true
]

{ #category : 'accessing' }
NECPreferences class >> enabled [
	^ enabled
]

{ #category : 'accessing' }
NECPreferences class >> enabled: aBoolean [
	enabled := aBoolean
]

{ #category : 'accessing' }
NECPreferences class >> expandPrefixes [
	^ expandPrefixes
]

{ #category : 'accessing' }
NECPreferences class >> expandPrefixes: aBoolean [
	expandPrefixes := aBoolean
]

{ #category : 'class initialization' }
NECPreferences class >> initialize [
	enabled := true.
	caseSensitive := true.
	smartCharacters := true.
	expandPrefixes := true.
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
	showCompletionDetails := self defaultShowCompletionDetails.
	NECEntry spaceAfterCompletion: true
]

{ #category : 'accessing' }
NECPreferences class >> popupAutomaticDelay [
	^ popupAutomaticDelay
]

{ #category : 'accessing' }
NECPreferences class >> popupAutomaticDelay: anObject [
	popupAutomaticDelay := anObject
]

{ #category : 'accessing' }
NECPreferences class >> popupShowAutomatic [
	^ popupShowAutomatic
]

{ #category : 'accessing' }
NECPreferences class >> popupShowAutomatic: aBoolean [
	popupShowAutomatic := aBoolean
]

{ #category : 'accessing' }
NECPreferences class >> popupShowWithShortcut [
	^ popupShowWithShortcut
]

{ #category : 'accessing' }
NECPreferences class >> popupShowWithShortcut: anObject [
	popupShowWithShortcut := anObject
]

{ #category : 'accessing' }
NECPreferences class >> showCompletionDetails [
	^ showCompletionDetails
]

{ #category : 'accessing' }
NECPreferences class >> showCompletionDetails: aBoolean [
	showCompletionDetails := aBoolean
]

{ #category : 'accessing' }
NECPreferences class >> smartCharacters [
	^ smartCharacters
]

{ #category : 'accessing' }
NECPreferences class >> smartCharacters: aBoolean [
	smartCharacters := aBoolean
]

{ #category : 'accessing' }
NECPreferences class >> smartCharactersMapping [
	^ smartCharactersMapping
]

{ #category : 'accessing' }
NECPreferences class >> smartCharactersMapping: aDictionary [
	smartCharactersMapping := aDictionary
]

{ #category : 'accessing' }
NECPreferences class >> smartCharactersWithDoubleSpace [
	^ smartCharactersWithDoubleSpace
]

{ #category : 'accessing' }
NECPreferences class >> smartCharactersWithDoubleSpace: aString [
	smartCharactersWithDoubleSpace := aString
]

{ #category : 'accessing' }
NECPreferences class >> smartCharactersWithSingleSpace [
	^ smartCharactersWithSingleSpace
]

{ #category : 'accessing' }
NECPreferences class >> smartCharactersWithSingleSpace: aString [
	smartCharactersWithSingleSpace := aString
]

{ #category : 'accessing' }
NECPreferences class >> spaceAfterCompletion [

	^ NECEntry spaceAfterCompletion
]

{ #category : 'accessing' }
NECPreferences class >> theme [
	self flag: #SmalltalkReference.
	^ Smalltalk ui theme
]

{ #category : 'accessing' }
NECPreferences class >> useEnterToAccept [
	^ useEnterToAccept
]

{ #category : 'accessing' }
NECPreferences class >> useEnterToAccept: aBoolean [
	useEnterToAccept := aBoolean
]
