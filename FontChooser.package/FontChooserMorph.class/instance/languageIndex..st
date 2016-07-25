accessing
languageIndex: anIndex

	anIndex isZero ifTrue: [ ^ self ]. 
	selectedPangram := (self pangrams at: anIndex) value.
	languageList selectionIndex: anIndex.
	self updatePreview