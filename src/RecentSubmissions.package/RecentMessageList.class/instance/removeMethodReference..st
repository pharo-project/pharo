actions
removeMethodReference: aMethodReference

	self methodReferenceList remove: aMethodReference.
	self updateView.