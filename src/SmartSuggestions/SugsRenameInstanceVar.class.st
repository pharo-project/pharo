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

{ #category : #display }
SugsRenameInstanceVar >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #display }
SugsRenameInstanceVar >> keyText [
	^'r'
]

{ #category : #display }
SugsRenameInstanceVar >> label [
	^'Rename' translated
]

{ #category : #display }
SugsRenameInstanceVar >> position [
	^10
]
