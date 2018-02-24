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

{ #category : #display }
SugsRenameClassVar >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #display }
SugsRenameClassVar >> keyText [
	^'r'
]

{ #category : #display }
SugsRenameClassVar >> label [
	^ 'Rename' translated
]

{ #category : #display }
SugsRenameClassVar >> position [
	^10
]
