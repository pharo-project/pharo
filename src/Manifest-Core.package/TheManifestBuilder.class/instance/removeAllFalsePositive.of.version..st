adding/removing
removeAllFalsePositive: aSet of: ruleId version: versionId

	| selector |
	selector := self selectorFalsePositiveOf:ruleId version: versionId. 
	self removeAllItem: aSet selector: selector.
	