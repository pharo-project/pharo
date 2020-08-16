"
A CompletionDoubleQuoteKeyboardEventTest is a test class for testing the behavior of CompletionDoubleQuoteKeyboardEvent
"
Class {
	#name : #CompletionDoubleQuoteKeyboardEventTest,
	#superclass : #TestCase,
	#instVars : [
		'completionDoubleQuoteKeyboardEvent',
		'replacements'
	],
	#category : #'NECompletion-Tests'
}

{ #category : #running }
CompletionDoubleQuoteKeyboardEventTest >> setUp [
	| event |
	super setUp.
	
	event := KeyboardEvent new.
	event
		setType: #keystroke
		buttons: nil
		position:  0@0
		keyValue: $"
		charCode: $" asciiValue
		hand: nil 
		stamp: Time now.
	
	completionDoubleQuoteKeyboardEvent := CompletionKeyboardEvent fromEvent: event editor: nil preferencesClass: NECPreferences.
	
	replacements := { 
		"Basic case"
		'foobar' -> '"foobar"'.
		"Basic case with spaces"
		'lorem ipsum' -> '"lorem ipsum"'.
		"With newlines"
'code.
"comment"
code.'
->
'"code.
""comment""
code."'.
		"With odd number of double quotes"
		'odd " number' -> '"odd "" number"'.
		'odd """ number' -> '"odd """""" number"'.
		
		"With only double quotes"
		'"' -> '""""'
	}
]

{ #category : #test }
CompletionDoubleQuoteKeyboardEventTest >> testCanUnencloseLeftRight [
	self assert: (completionDoubleQuoteKeyboardEvent canUnencloseLeft: $" right: $").
	
	"Should handle nil since left of selection could be nil if everything in a text area is selected"
	self deny: (completionDoubleQuoteKeyboardEvent canUnencloseLeft: nil right: $").
	self deny: (completionDoubleQuoteKeyboardEvent canUnencloseLeft: $" right: nil).
	
	"Should not work if the characters don't match"
	self deny: (completionDoubleQuoteKeyboardEvent canUnencloseLeft: $a right: $b).
	
	"Should work for paired characters"
	"TODO move this to the CompletionLeftParenKeyboardEventTest"
	"self assert: (completionDoubleQuoteKeyboardEvent canUnencloseLeft: $( right: $))."
	
	"Should not work if the characters are flipped"
	"TODO move this to the CompletionLeftBracketKeyboardEventTest"
	"self deny: (completionDoubleQuoteKeyboardEvent canUnencloseLeft: $] right: $[)."
]

{ #category : #test }
CompletionDoubleQuoteKeyboardEventTest >> testFromEventEditorPreferencesClass [
	self assert: completionDoubleQuoteKeyboardEvent class equals: CompletionDoubleQuoteKeyboardEvent 
]

{ #category : #test }
CompletionDoubleQuoteKeyboardEventTest >> testKeyCharacterLeft [
	self assert: completionDoubleQuoteKeyboardEvent keyCharacterLeft equals: $"
]

{ #category : #test }
CompletionDoubleQuoteKeyboardEventTest >> testKeyCharacterRight [
	self assert: completionDoubleQuoteKeyboardEvent keyCharacterRight equals: $"
]

{ #category : #test }
CompletionDoubleQuoteKeyboardEventTest >> testReplacementAfterEnclosingSelection [
	replacements
		do: [ :pair | 
			self 
				assert: (completionDoubleQuoteKeyboardEvent replacementAfterEnclosingSelection: pair key)
				equals: pair value 
		]
]

{ #category : #test }
CompletionDoubleQuoteKeyboardEventTest >> testReplacementAfterUnenclosingSelection [
	replacements
		do: [ :pair | 
			self 
				assert: (completionDoubleQuoteKeyboardEvent replacementAfterUnenclosingSelection: pair value)
				equals: pair key 
		]
]
