"
I know how to rename a local variable.
"
Class {
	#name : #SugsRenameTemporaryVar,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsRenameTemporaryVar class >> nodes [
	^{RBTemporaryNode}
]

{ #category : #execution }
SugsRenameTemporaryVar >> execute [
	(NautilusRefactoring model: context)
				renameTemporaryNamed: context selectedVariableName
				Between: (context selectedNode start to: context selectedNode stop)
				from: context selectedMethod
]

{ #category : #display }
SugsRenameTemporaryVar >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #display }
SugsRenameTemporaryVar >> keyText [
	^'r'
]

{ #category : #display }
SugsRenameTemporaryVar >> label [
	^  'Rename' translated
]

{ #category : #display }
SugsRenameTemporaryVar >> position [
	^10
]
