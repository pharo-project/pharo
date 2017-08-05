tests
testGuidedBan

	| crit |
	
	crit := ReTrivialCritique
		for: thisContext method
		by: (RBLintRule allSubclasses detect: #isVisible) new.
	
	self runWithDialogPopping: [
		self shouldnt: [ crit guidedBan ] raise: Error ]
	