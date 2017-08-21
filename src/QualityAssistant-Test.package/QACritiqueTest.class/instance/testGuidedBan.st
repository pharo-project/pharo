as yet unclassified
testGuidedBan

	| crit |
	
	"This tests should be rewritten: it only runs ok in graphical mode. In headless it creates sometimes new windows and sometimes it does not (depending if it uses UIManager of directly Nautilus)."
	self flag: #bug.
	self skip.
	
	crit := ReTrivialCritique
		for: thisContext method
		by: (RBLintRule allSubclasses detect: #isVisible) new.
	
	self runWithDidalogPopping: [
		self shouldnt: [ crit guidedBan ] raise: Error ]
	