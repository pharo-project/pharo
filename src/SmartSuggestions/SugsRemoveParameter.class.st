"
I know how to remove a parameter from a method
"
Class {
	#name : #SugsRemoveParameter,
	#superclass : #SugsSuggestion,
	#category : #SmartSuggestions-Suggestion
}

{ #category : #execution }
SugsRemoveParameter >> execute [
	(NautilusRefactoring model: context) removeParameterFor: context selectedMethod 
]

{ #category : #execution }
SugsRemoveParameter >> isValidForContext [
	^ context hasParameters
]

{ #category : #accessing }
SugsRemoveParameter >> label [
	^'Remove parameter' translated
]
