adding/removing
removeFalsePositive: fp of: ruleId version: versionId
	
	| selector |
	selector := self selectorFalsePositiveOf: ruleId version: versionId. 
	self removeItem: fp selector: selector.
	
	