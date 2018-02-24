"
I know how to rename a class
"
Class {
	#name : #SugsRenameClass,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsRenameClass >> execute [
	(NautilusRefactoring model: context) renameClass: context bindedSelectedClass.
]

{ #category : #accessing }
SugsRenameClass >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #accessing }
SugsRenameClass >> keyText [
	^'r'
]

{ #category : #accessing }
SugsRenameClass >> label [
	^'Rename class' translated
]

{ #category : #accessing }
SugsRenameClass >> position [
	^20
]
