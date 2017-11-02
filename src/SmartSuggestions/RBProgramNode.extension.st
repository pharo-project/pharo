Extension { #name : #RBProgramNode }

{ #category : #'*SmartSuggestions' }
RBProgramNode >> specialCommands [
	^#().
]

{ #category : #'*SmartSuggestions' }
RBProgramNode >> suggestions [
	^SugsSuggestionFactory globalCommands addAll: self specialCommands; yourself 
]

{ #category : #'*SmartSuggestions' }
RBProgramNode >> sugsMenuLabel [
	^ self formattedCode
]
