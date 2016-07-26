visitor
visitLogCommentModification: anEvent 
	^ anEvent copyWithEntryReferences: referencesMap values.