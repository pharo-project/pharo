updating
updateSelectionInterval

	self selectedContext ifNil: [ ^ self ].
	self selectionInterval: (self session selectedCodeRangeForContext:  self selectedContext )