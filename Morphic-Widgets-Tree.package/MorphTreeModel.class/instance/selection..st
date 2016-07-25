selecting
selection: aSelection
	self setSelection: aSelection.
	self  requestView: MorphTreeChangeRequest updateSelectionFromModel
