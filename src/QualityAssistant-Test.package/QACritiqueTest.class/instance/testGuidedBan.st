as yet unclassified
testGuidedBan

	| crit |
	
	crit := ReTrivialCritique
		for: thisContext method
		by: (RBLintRule allSubclasses detect: #isVisible) new.
	
	self runWithDidalogPopping: [
		self shouldnt: [ crit guidedBan ] raise: Error ]
	