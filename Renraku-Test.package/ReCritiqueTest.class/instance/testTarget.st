tests
testTarget
	| rule critic |
	
	rule := (RBLintRule allSubclasses select: #isVisible)	 anyOne new.
	critic := ReAbstractCritique for: thisContext method by: rule.
	
	self assert: critic sourceAnchor entity equals: thisContext method