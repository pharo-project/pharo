"
I know how to rename methods
"
Class {
	#name : #SugsRenameMethod,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsRenameMethod >> execute [
	 (NautilusRefactoring model: context) renameMethodFor: context selectedMethod 
]

{ #category : #accessing }
SugsRenameMethod >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #accessing }
SugsRenameMethod >> keyText [
	^'r,m'
]

{ #category : #accessing }
SugsRenameMethod >> label [
	^ 'Rename' translated
]

{ #category : #accessing }
SugsRenameMethod >> position [
	^10
]
