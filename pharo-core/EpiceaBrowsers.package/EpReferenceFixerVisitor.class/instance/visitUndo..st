visitor
visitUndo: anUndoEvent
	^ anUndoEvent copyWithEntryReferences: referencesMap values.