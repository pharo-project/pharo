instance creation
entryReferences: aCollection oldComment: aString newComment: anotherString
	^ self basicNew
		initializeWithEntryReferences: aCollection 
		oldComment: aString 
		newComment: anotherString;
		yourself