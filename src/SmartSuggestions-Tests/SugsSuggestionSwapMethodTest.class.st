"
SUnit tests for the swap method suggestion
"
Class {
	#name : #SugsSuggestionSwapMethodTest,
	#superclass : #SugsSuggestionTest,
	#category : #SmartSuggestions-Tests-Tests
}

{ #category : #tests }
SugsSuggestionSwapMethodTest >> testLabelForSwapClassMethodRefersToInstance [

	| sugg |
	sugg := SugsSuggestionFactory createSwapMethod.
	sugg context: SugsMockContext methodClassContext.
	
	self assert: sugg label equals: 'Move to instance side' translated.
]

{ #category : #tests }
SugsSuggestionSwapMethodTest >> testLabelForSwapInstanceMethodRefersToClass [
	| sugg |
	sugg := SugsSuggestionFactory createSwapMethod.
	sugg context: SugsMockContext methodContext.
	
	self assert: sugg label equals: 'Move to class side' translated.
]

{ #category : #tests }
SugsSuggestionSwapMethodTest >> testLabelForSwapWithoutSelectedMethodIsGeneric [
	| sugg |
	sugg := SugsSuggestionFactory createSwapMethod.
	sugg context: SugsMockContext new.
	
	self assert: sugg label equals: 'Swap method' translated.
]
