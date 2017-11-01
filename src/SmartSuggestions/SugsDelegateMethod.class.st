"
I know how to delegate the selected method in an instance variable and move the code to a class
"
Class {
	#name : #SugsDelegateMethod,
	#superclass : #SugsSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsDelegateMethod >> execute [
	 (NautilusRefactoring model: context) moveMethodFor: context selectedMethod
]

{ #category : #execution }
SugsDelegateMethod >> isValidForContext [
	^ context hasInstanceVariable
]

{ #category : #accessing }
SugsDelegateMethod >> label [
	^'Delegate selected method' translated
]
