"
This inline a variable in an assignment sentence
"
Class {
	#name : #SugsInlineTemporary,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsInlineTemporary class >> nodes [
	^{RBAssignmentNode}
]

{ #category : #execution }
SugsInlineTemporary >> execute [
	 (NautilusRefactoring model: context) inlineTemporaryBetween: context selectedInterval from: context selectedMethod
]

{ #category : #display }
SugsInlineTemporary >> icon [
	^self iconNamed: #smallLanguageIcon
]

{ #category : #display }
SugsInlineTemporary >> label [
	 ^'Inline temporary' translated
]

{ #category : #display }
SugsInlineTemporary >> position [
	^10
]
