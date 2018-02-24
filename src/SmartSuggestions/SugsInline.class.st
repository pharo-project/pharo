"
I know how to inline the method from a message
"
Class {
	#name : #SugsInline,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsInline class >> nodes [
	^{RBMessageNode}
]

{ #category : #execution }
SugsInline >> execute [
	 (NautilusRefactoring model: context) inlineMethodBetween: context selectedInterval from: context selectedMethod
]

{ #category : #display }
SugsInline >> icon [
	^self iconNamed: #smallRightFlushIcon
]

{ #category : #display }
SugsInline >> label [
	 ^'Inline method' translated
]

{ #category : #display }
SugsInline >> position [
	^100
]
