instance creation
onEnvironment: anEnvironment keywords: aKeywordCollection
	^ (self onEnvironment: anEnvironment)
		keywords: aKeywordCollection;
		yourself