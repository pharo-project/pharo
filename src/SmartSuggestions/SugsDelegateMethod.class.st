"
I know how to delegate the selected method in an instance variable and move the code to a class
"
Class {
	#name : #SugsDelegateMethod,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsDelegateMethod class >> nodes [
	^{RBMethodNode}
]

{ #category : #execution }
SugsDelegateMethod >> execute [
	 (NautilusRefactoring model: context) moveMethodFor: context selectedMethod
]

{ #category : #display }
SugsDelegateMethod >> icon [
	^self iconNamed: #smallRedoIcon
]

{ #category : #execution }
SugsDelegateMethod >> isValidForContext [
	^ context selectedClass allInstVarNames isNotEmpty
]

{ #category : #display }
SugsDelegateMethod >> label [
	^'Delegate selected method' translated
]

{ #category : #display }
SugsDelegateMethod >> position [
	^210
]
