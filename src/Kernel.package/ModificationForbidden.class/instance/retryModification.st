retrying
retryModification
	object perform: retrySelector with: fieldIndex with: newValue.
	self resume: newValue