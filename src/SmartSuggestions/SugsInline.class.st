"
I know how to inline the method from a message
"
Class {
	#name : #SugsInline,
	#superclass : #SugsSuggestion,
	#category : #SmartSuggestions-Suggestion
}

{ #category : #execution }
SugsInline >> execute [
	 (NautilusRefactoring model: context) inlineMethodBetween: context selectedInterval from: context selectedMethod
]

{ #category : #accessing }
SugsInline >> label [
	 ^'Inline method' translated
]
