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

{ #category : #display }
SugsExtractMethod >> icon [
	^self iconNamed: #smallUpdateIcon
]

{ #category : #execution }
SugsExtractMethod >> isValidForContext [
	^ context hasMoreSentences
]

{ #category : #display }
SugsExtractMethod >> label [
	^'Extract method' translated
]

{ #category : #display }
SugsExtractMethod >> position [
	^50
]
