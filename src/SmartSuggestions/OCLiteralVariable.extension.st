Extension { #name : #OCLiteralVariable }

{ #category : #'*SmartSuggestions' }
OCLiteralVariable >> specialCommands [
	^self assoc isClassVariable 
		ifTrue: [SugsSuggestionFactory commandsForClassVariable]
		ifFalse:[ ^SugsSuggestionFactory commandsForClass]
]
