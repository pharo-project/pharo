"
I know how to push down the method to my subclass
"
Class {
	#name : #SugsPushDownMethod,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsPushDownMethod >> execute [
	(NautilusRefactoring model: context) pushDownMethodsFor: (OrderedCollection with: context selectedMethod)
]

{ #category : #display }
SugsPushDownMethod >> icon [
	^self iconNamed: #smallUndoIcon
]

{ #category : #execution }
SugsPushDownMethod >> isValidForContext [
	^  context hasSubclasses
]

{ #category : #display }
SugsPushDownMethod >> label [
	^'Push Down' translated
]

{ #category : #display }
SugsPushDownMethod >> position [
	^101
]
