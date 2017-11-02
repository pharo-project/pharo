"
I know how to add a parameter to a method
"
Class {
	#name : #SugsAddParameter,
	#superclass : #SugsSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsAddParameter >> execute [
	(NautilusRefactoring model: context) addAParameterFor: context selectedMethod
]

{ #category : #accessing }
SugsAddParameter >> label [
	^'Add a parameter' translated
]
