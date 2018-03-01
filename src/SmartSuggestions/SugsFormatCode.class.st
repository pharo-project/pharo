"
I know how to format the code
"
Class {
	#name : #SugsFormatCode,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsFormatCode >> execute [
	context formatSourceCode
]

{ #category : #accessing }
SugsFormatCode >> label [
	^'Format method' translated
]
