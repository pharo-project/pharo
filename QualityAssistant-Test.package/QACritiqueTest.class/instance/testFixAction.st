accessing
testFixAction

	| crit action |
	
	crit := ReTransformationCritique
		for: thisContext method
		by: (RBTransformationRule allSubclasses detect: #isVisible) new
		tree: thisContext method parseTree.
	
	action := crit actions detect: [ :a | a description = 'Automatically resolve the issue' ].
	
	self runWithDidalogPopping: [
		self
			shouldnt: [ action actOnCritic: crit ofEntity: crit sourceAnchor entity ]
			raise: Error ]