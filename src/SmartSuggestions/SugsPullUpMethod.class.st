"
I know how to pull up the method to my superclass
"
Class {
	#name : #SugsPullUpMethod,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsPullUpMethod >> execute [
	(NautilusRefactoring model: context) pullUpMethodsFor: (OrderedCollection with: context selectedMethod)
]

{ #category : #display }
SugsPullUpMethod >> icon [
	^self iconNamed: #smallRedoIcon
]

{ #category : #display }
SugsPullUpMethod >> label [
	^ 'Pull Up' translated
]

{ #category : #display }
SugsPullUpMethod >> position [
	^100
]
