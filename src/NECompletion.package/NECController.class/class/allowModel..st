testing
allowModel: aModel 
	^NECPreferences enabled and: [ aModel isCodeCompletionAllowed ]