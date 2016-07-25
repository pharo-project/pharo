tests
testRule
	| rule critic |
	
	rule := (RBLintRule allSubclasses select: #isVisible)	 anyOne new.
	critic := ReAbstractCritique for: thisContext method by: rule.
	
	self assert: critic rule equals: rule