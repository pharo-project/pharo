Class {
	#name : #CompletionKeyboardEvent,
	#superclass : #Object,
	#instVars : [
		'event',
		'editor',
		'preferencesClass'
	],
	#category : #'NECompletion-Morphic'
}

{ #category : #private }
CompletionKeyboardEvent class >> determineSubclassFromEvent: aKeyboardEvent [
	| char |
	char := aKeyboardEvent keyCharacter.
	
	(char = $")
		ifTrue: [ ^ CompletionDoubleQuoteKeyboardEvent ].
		
	^ CompletionDefaultKeyboardEvent
		
]

{ #category : #'instance creation' }
CompletionKeyboardEvent class >> fromEvent: aKeyboardEvent editor: anEditor preferencesClass: preferencesClass [
	"This is a factory method that builds the correct subtype of CompletionKeyboardEvent"
	^ (self determineSubclassFromEvent: aKeyboardEvent) new
		event: aKeyboardEvent;
		editor: anEditor;
		preferencesClass: preferencesClass;
		yourself 
]

{ #category : #accessing }
CompletionKeyboardEvent >> editor [
	^ editor
]

{ #category : #accessing }
CompletionKeyboardEvent >> editor: anObject [
	editor := anObject
]

{ #category : #accessing }
CompletionKeyboardEvent >> event [
	^ event
]

{ #category : #accessing }
CompletionKeyboardEvent >> event: anObject [
	event := anObject
]

{ #category : #queries }
CompletionKeyboardEvent >> keyCharacterLeft [
	^ self preferencesClass smartCharactersLeftFor: self event keyCharacter 
]

{ #category : #queries }
CompletionKeyboardEvent >> keyCharacterRight [
	^ self preferencesClass smartCharactersRightFor: self event keyCharacter 
]

{ #category : #accessing }
CompletionKeyboardEvent >> preferencesClass [
	^ preferencesClass
]

{ #category : #accessing }
CompletionKeyboardEvent >> preferencesClass: anObject [
	preferencesClass := anObject
]

{ #category : #'actions - enclosing' }
CompletionKeyboardEvent >> replacementAfterEnclosingSelection: selectedString [
	"Returns the replacement for the selectedString after enclosing takes place for this CompletionKeyboardEvent
	
	This method describes the default behavior, which is to return the original selection"
	
	^ selectedString 
]

{ #category : #'actions - enclosing' }
CompletionKeyboardEvent >> replacementAfterUnenclosingSelection: selectedString [
	"Returns the replacement for the selectedString after unenclosing takes place for this CompletionKeyboardEvent
	
	This method describes the default behavior, which is to return the original selection"
	
	^ selectedString 
]
