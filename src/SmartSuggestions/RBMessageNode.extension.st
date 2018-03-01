Extension { #name : #RBMessageNode }

{ #category : #'*SmartSuggestions' }
RBMessageNode >> specialCommands [
	^ SugsSuggestionFactory commandsForMessage
]

{ #category : #'*SmartSuggestions' }
RBMessageNode >> sugsMenuLabel [
	^ self selector
]
