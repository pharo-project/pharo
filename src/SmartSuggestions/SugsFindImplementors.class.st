"
I know how to find implementors for a message
"
Class {
	#name : #SugsFindImplementors,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #accessing }
SugsFindImplementors class >> nodes [
	^{RBMessageNode}
]

{ #category : #execution }
SugsFindImplementors >> execute [
	SystemNavigation default browseAllImplementorsOf: context selectedMessageName
				
]

{ #category : #display }
SugsFindImplementors >> icon [
	^self iconNamed: #smallFindIcon
]

{ #category : #display }
SugsFindImplementors >> keyText [
	^'m'
]

{ #category : #display }
SugsFindImplementors >> label [
	^  'Implementors' translated
]

{ #category : #display }
SugsFindImplementors >> position [
	^11
]
