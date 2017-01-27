retrying
retryModification
	object perform: selector with: index with: value.
	self resume: value