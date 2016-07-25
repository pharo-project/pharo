adding/removing
addTruePositive: fp of: ruleId version: versionId

	| selector  |	
	selector := self selectorTruePositiveOf: ruleId version: versionId. 
	self addItem: fp selector: selector.
	(self containsToDo: fp onRule: ruleId version: versionId)
		ifTrue: [ self removeToDo: fp of:  ruleId version: versionId ]. 
	(self containsFalsePositive: fp onRule: ruleId version: versionId)
		ifTrue: [ self removeFalsePositive: fp of:  ruleId version: versionId ]

	
	