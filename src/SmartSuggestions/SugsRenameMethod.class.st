"
I know how to rename methods
"
Class {
	#name : #SugsRenameMethod,
	#superclass : #SugsSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsRenameMethod >> execute [
	 (NautilusRefactoring model: context) renameMethodFor: context selectedMethod 
]

{ #category : #accessing }
SugsRenameMethod >> label [
	^ 'Rename' translated
]
