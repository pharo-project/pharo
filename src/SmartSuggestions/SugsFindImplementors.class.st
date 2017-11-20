"
I know how to find implementors for a message
"
Class {
	#name : #SugsFindImplementors,
	#superclass : #SugsSuggestion,
	#category : #SmartSuggestions-Suggestion
}

{ #category : #execution }
SugsFindImplementors >> execute [
	SystemNavigation default browseAllImplementorsOf: context selectedMessageName
				
]

{ #category : #accessing }
SugsFindImplementors >> label [
	^  'Implementors' translated
]
