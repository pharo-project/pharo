actions
conflictSelectionDo: aBlock
	self selectionIsConflicted
		ifTrue: aBlock
		ifFalse: [self inform: 'You must have a conflict selected']