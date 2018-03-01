"
I know how to rename an instance variable
"
Class {
	#name : #SugsRenameInstanceVar,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsRenameInstanceVar >> execute [
	(NautilusRefactoring model: context) renameInstVarNamed: context selectedVariableName from: context selectedClass
]

{ #category : #accessing }
SugsRenameInstanceVar >> label [
	^'Rename' translated
]
