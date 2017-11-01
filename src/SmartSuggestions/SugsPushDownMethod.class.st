"
I know how to push down the method to my subclass
"
Class {
	#name : #SugsPushDownMethod,
	#superclass : #SugsSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsPushDownMethod >> execute [
	(NautilusRefactoring model: context) pushDownMethodsFor: (OrderedCollection with: context selectedMethod)
]

{ #category : #execution }
SugsPushDownMethod >> isValidForContext [
	^  context hasSubclasses
]

{ #category : #accessing }
SugsPushDownMethod >> label [
	^'Push Down' translated
]
