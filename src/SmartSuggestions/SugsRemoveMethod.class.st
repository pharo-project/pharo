"
I know how to remove a mehod
"
Class {
	#name : #SugsRemoveMethod,
	#superclass : #SugsSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsRemoveMethod >> execute [
	(NautilusRefactoring model: context) removeMethodsFor: (OrderedCollection with: context selectedMethod)
]

{ #category : #accessing }
SugsRemoveMethod >> label [
	^'Remove method' translated
]
