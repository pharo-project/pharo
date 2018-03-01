Extension { #name : #RBMethodNode }

{ #category : #'*SmartSuggestions' }
RBMethodNode >> specialCommands [
	 ^SugsSuggestionFactory commandsForMethod.
]

{ #category : #'*SmartSuggestions' }
RBMethodNode >> sugsMenuLabel [
	^ self selector
]
