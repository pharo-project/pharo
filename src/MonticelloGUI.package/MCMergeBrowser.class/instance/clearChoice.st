actions
clearChoice
	self conflictSelectionDo:
		[selection clearChoice.
		self changed: #text; changed: #list]