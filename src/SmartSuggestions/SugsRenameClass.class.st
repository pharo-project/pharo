"
I know how to rename a class
"
Class {
	#name : #SugsRenameClass,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsRenameClass class >> nodes [
	^{RBGlobalNode}
]

{ #category : #execution }
SugsRenameClass >> execute [
	(NautilusRefactoring model: context) renameClass: context bindedSelectedClass.
]

{ #category : #display }
SugsRenameClass >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #execution }
SugsRenameClass >> isValidForContext [
	^context selectedNode binding value isClass
]

{ #category : #display }
SugsRenameClass >> keyText [
	^'r'
]

{ #category : #display }
SugsRenameClass >> label [
	^'Rename class' translated
]

{ #category : #display }
SugsRenameClass >> position [
	^20
]
