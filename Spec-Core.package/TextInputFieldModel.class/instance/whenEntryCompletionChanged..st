api-events
whenEntryCompletionChanged: aBlock
	"aBlock will be performed when entryCompletion changed"
	
	entryCompletion whenChangedDo: aBlock