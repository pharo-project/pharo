Extension { #name : #RBVariableNode }

{ #category : #'*SmartSuggestions' }
RBVariableNode >> specialCommands [
	^self binding specialCommands
]

{ #category : #'*SmartSuggestions' }
RBVariableNode >> sugsMenuLabel [
	^ name
]
