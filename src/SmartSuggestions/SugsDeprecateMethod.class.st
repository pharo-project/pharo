"
I know how to deprecate method.

"
Class {
	#name : #SugsDeprecateMethod,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsDeprecateMethod >> execute [
	(NautilusRefactoring model: context) deprecateMethodFor: context selectedMethod
]

{ #category : #accessing }
SugsDeprecateMethod >> label [
	^ 'Deprecate' translated
]
