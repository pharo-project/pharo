updating
updateSelectionInterval

	self selectedContext ifNil: [ ^ self ].
	self selectionInterval: (self session pcRangeForContext:  self selectedContext )