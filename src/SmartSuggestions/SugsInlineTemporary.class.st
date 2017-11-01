"
This inline a variable in an assignment sentence
"
Class {
	#name : #SugsInlineTemporary,
	#superclass : #SugsSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsInlineTemporary >> execute [
	 (NautilusRefactoring model: context) inlineTemporaryBetween: context selectedInterval from: context selectedMethod
]

{ #category : #accessing }
SugsInlineTemporary >> label [
	 ^'Inline temporary' translated
]
