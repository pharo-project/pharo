Extension { #name : #RBAssignmentNode }

{ #category : #'*SmartSuggestions' }
RBAssignmentNode >> specialCommands [
	^ SugsSuggestionFactory commandsForAssignment.
]
