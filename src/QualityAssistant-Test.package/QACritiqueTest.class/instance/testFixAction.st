accessing
testFixAction

	| crit action |
	
	"This tests should be rewritten: it only runs ok in graphical mode. In headless it creates sometimes new windows and sometimes it does not (depending if it uses UIManager of directly Nautilus)."
	self flag: #bug.
	self skip.
	
	crit := ReTransformationCritique
		for: thisContext method
		by: (RBTransformationRule allSubclasses detect: #isVisible) new
		tree: thisContext method parseTree.
	
	action := crit actions detect: [ :a | a description = 'Automatically resolve the issue' ].
	
	self runWithDidalogPopping: [
		self
			shouldnt: [ action actOnCritic: crit ofEntity: crit sourceAnchor entity ]
			raise: Error ]