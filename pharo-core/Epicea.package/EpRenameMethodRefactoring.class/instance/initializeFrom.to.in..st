initialize-release
initializeFrom: anOldSelector to: aNewSelector in: aClassName
	self initialize.
	
	oldSelector := anOldSelector.
	newSelector := aNewSelector.
	affectedClassName := aClassName