"
I know how to rename a local variable.
"
Class {
	#name : #SugsRenameTemporaryVar,
	#superclass : #SugsSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsRenameTemporaryVar >> execute [
	(NautilusRefactoring model: context)
				renameTemporaryNamed: context selectedVariableName
				Between: (context nodeStart to: context nodeStop)
				from: context selectedMethod
]

{ #category : #accessing }
SugsRenameTemporaryVar >> label [
	^  'Rename' translated
]
