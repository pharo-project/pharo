*SmartSuggestions
specialCommands
	^self assoc isClassVariable 
		ifTrue: [SugsSuggestionFactory commandsForClassVariable]
		ifFalse:[ ^SugsSuggestionFactory commandsForClass]