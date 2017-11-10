Extension { #name : #RBBlockNode }

{ #category : #'*SmartSuggestions' }
RBBlockNode >> specialCommands [
	^ SugsSuggestionFactory commandsForBlock
]
