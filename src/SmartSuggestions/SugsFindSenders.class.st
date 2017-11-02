"
I know how to find senders from a message
"
Class {
	#name : #SugsFindSenders,
	#superclass : #SugsSuggestion,
	#category : 'SmartSuggestions-Suggestion'
}

{ #category : #execution }
SugsFindSenders >> execute [
	SystemNavigation default
				browseSendersOf: context selectedMessageName
				name: 'Senders of ' translated , context selectedMessageName
				autoSelect: nil
]

{ #category : #accessing }
SugsFindSenders >> label [
	^ 'Senders' translated
]
