"
The classic option to extract a line of code in a local variable
"
Class {
	#name : #SugsExtractToLocalVariable,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsExtractToLocalVariable >> execute [
	(NautilusRefactoring model: context) extractToTempTextSelection
]

{ #category : #execution }
SugsExtractToLocalVariable >> isValidForContext [
	^context hasOneStatement.
]

{ #category : #accessing }
SugsExtractToLocalVariable >> label [
	^ 'Extract local' translated
]
