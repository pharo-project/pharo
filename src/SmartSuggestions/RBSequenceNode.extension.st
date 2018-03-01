Extension { #name : #RBSequenceNode }

{ #category : #'*SmartSuggestions' }
RBSequenceNode >> specialCommands [ 
	^ SugsSuggestionFactory commandsForSource.
]
