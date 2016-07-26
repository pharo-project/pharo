copying
copyWithEntryReferences: aCollection
	^ self class
		entryReferences: aCollection
		oldComment: self oldComment
		newComment: self newComment