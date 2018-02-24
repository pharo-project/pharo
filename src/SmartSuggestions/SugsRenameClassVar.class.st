"
I know how to rename a class variable
"
Class {
	#name : #SugsRenameClassVar,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsRenameClassVar >> execute [
	(NautilusRefactoring model: context) renameClassVarNamed: context selectedVariableName from: context selectedClass
]

{ #category : #accessing }
SugsRenameClassVar >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #accessing }
SugsRenameClassVar >> keyText [
	^'r'
]

{ #category : #accessing }
SugsRenameClassVar >> label [
	^ 'Rename' translated
]

{ #category : #accessing }
SugsRenameClassVar >> position [
	^10
]
