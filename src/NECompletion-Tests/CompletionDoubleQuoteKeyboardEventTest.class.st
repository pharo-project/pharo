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
		'odd """ number' -> '"odd """""" number"'
	}
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
CompletionDoubleQuoteKeyboardEventTest >> testReplacementAfterEncloseActionOnSelection [
	replacements
		do: [ :pair | 
			"Enclosing should work"
			self 
				assert: (completionDoubleQuoteKeyboardEvent replacementAfterEncloseActionOnSelection: pair key)
				equals: pair value.
			"Undoing should work too"
			self 
				assert: (completionDoubleQuoteKeyboardEvent replacementAfterEncloseActionOnSelection: pair value)
				equals: pair key.
		]
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
