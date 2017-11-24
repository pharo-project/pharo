"
If you want to extract code in a new method I'm your guy
"
Class {
	#name : #SugsExtractMethod,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsExtractMethod >> execute [
	(NautilusRefactoring model: context) extractToMethodTextSelection
]

{ #category : #execution }
SugsExtractMethod >> isValidForContext [
	^ context hasMoreSentences
]

{ #category : #accessing }
SugsExtractMethod >> label [
	^'Extract method' translated
]
