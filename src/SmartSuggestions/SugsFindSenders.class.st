"
I know how to find senders from a message
"
Class {
	#name : #SugsFindSenders,
	#superclass : #SugsSuggestion,
	#category : #'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsFindSenders >> execute [
	SystemNavigation default
				browseSendersOf: context selectedMessageName
				name: 'Senders of ' translated , context selectedMessageName
				autoSelect: nil
]

{ #category : #display }
SugsFindSenders >> icon [
	^self iconNamed: #smallFindIcon
]

{ #category : #display }
SugsFindSenders >> keyText [
	^'n'
]

{ #category : #display }
SugsFindSenders >> label [
	^ 'Senders' translated
]

{ #category : #display }
SugsFindSenders >> position [
	^10
]
