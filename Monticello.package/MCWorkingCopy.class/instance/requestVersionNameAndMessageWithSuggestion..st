private
requestVersionNameAndMessageWithSuggestion: aString
	^ (MCVersionNameAndMessageRequest new suggestedName: aString) signal