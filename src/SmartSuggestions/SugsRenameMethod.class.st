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

{ #category : #display }
SugsRenameMethod >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #display }
SugsRenameMethod >> keyText [
	^'r,m'
]

{ #category : #display }
SugsRenameMethod >> label [
	^ 'Rename' translated
]

{ #category : #display }
SugsRenameMethod >> position [
	^10
]
